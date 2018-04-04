


reqAndAssign = function(obj, name) {
  req(obj)
  assign(name, obj, pos = 1L)
}


validateData = function(df) {
  validate(need(class(df) == "data.frame", "You didn't import any data."))
}

reqNFeat = function(feat.sel, df) {
  req(all(feat.sel %in% colnames(df)))
}



addPlotTheme = function(plot.obj) {
  plot.theme = theme(axis.line = element_line(size = 1, colour = "black"),
                     panel.grid.major = element_line(colour = "#d3d3d3"),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.title = element_blank(),
                     axis.text.x = element_text(colour = "black", size = 9),
                     axis.title.x = element_text(vjust = 0.9),
                     axis.text.y = element_text(colour = "black", size = 9))
  plot.obj + theme_bw() + plot.theme
}


generateFunctionList <- function(functions = vector() , pattern = vector())
{
  # initialize a list of functions
  functionList <- list()
  
 
    # for every function in functions and for every pattern a list entrie is generated
    for(i in 1:length(functions)){
      
      npat <- pattern[i]
      nfunc <- functions[i]
      
      # assigning the function to the list element
      functionList[[paste(nfunc , npat ,sep = "_")]] <- try(eval(parse(text = nfunc)) , silent = T)
      
      # setting the pattern in the function
      formals(functionList[[paste(nfunc , npat ,sep = "_")]])[["pattern"]] <- npat
      
      
    }
    
 
  return(functionList)
  
}




#function which calls all functions within the function list and evaluates it on the specified data
evaluateFunList <- function(funList , data){
  
  #For every entrie in funList call the function on data
  for(i in 1:length(funList)){
    
    # mainly nessesary in case the user uploads a self defined function
    data <- try(funList[[i]](data) , silent = T)
    
  }
  
  return(data)
  
} 

# a function which assigns NA to every infinite value

removeInfinites <- function(data){
  
  for(n in 1:dim(data)[2])
  {
    if(any(!is.finite(data[,n])))
    {
      data[!is.finite(data[,n]) , n ] <- NA
    }
    
  }
  
}


removeNAs <- function(data){
  
  idxNA <- apply(data , 1 , function(x) any( is.na(x) ) )
  
  data <- data[!idxNA,]
}



#TOdo: write an initial function to estimate the us.rate

generateModel <- function(classifier , us.rate , features , data , targetVariable , positiveClass , hyperpars = list() , estimatingThreshold = F , tprThreshold = 0.995){
  
  #InitialTrainData to have it consistent with testdata
  
  initialData <- data
  
  #Generating the initial Learner
  learner <- makeLearner(classifier , predict.type = "prob" , par.vals = hyperpars)
  
  #normalizing the data
  learner <- makePreprocWrapperCaret(learner ,  ppc.scale = TRUE , ppc.center = TRUE)
  
  #create an undersample wrapper
  learner <- makeUndersampleWrapper(learner , usw.rate = us.rate)
  
  # subsetting the data to those variables which are needed
  data <- data[, c(features , targetVariable) ]
  
  # removing all rows which contains any NA
  data <- removeNAs( data = data )
  
  #converting character and logicals to factors and integers to numerics
  data <- convertClass( data )
  
  # generating the task for the model
  task <- makeClassifTask( data = data , target = targetVariable , positive = positiveClass )
  
  
  # if the Threshold should be estimated a CV is run and the threshold is returned
  if(estimatingThreshold){
    
    # resample Description
    resamp <- mlr::makeResampleDesc("CV" , iters = 10 ,stratify = F , predict = "both")
    
    # performing the resampling
    resampResult <- mlr::resample(learner , task , resamp , measure = list(tpr , fpr , acc , ppv , auc) , models = T)
    
    # calculating the Threshold based on the prediction of the 10 fold CV
    optimalthreshold <- getThresholdCV(resampleResult = resampResult , train.data = data , tprThreshold = tprThreshold)
    
    
  }else{
    
    optimalthreshold <- NULL
    
  }
  
  #training the model
  model <- mlr::train(learner = learner , task = task)
  
  
  output <- list(model = model , optimalthreshold = optimalthreshold , train.data = initialData , task = task , learner = learner)
  
  return(output)
  
  
}


# function which generates a new list of functions
combineModel <- function(trainOutput , featureFunctionList , test.data = NULL ,  threshold = NULL , positveClass = NULL){
  
  combinedModel <- list()
  
  combinedModel[["model"]] <- trainOutput$model
  
  combinedModel[["funList"]] <- featureFunctionList
  
  if(is.null(threshold)){
    
    combinedModel[["threshold"]] <- trainOutput$optimalthreshold
    
  }else{
    combinedModel[["threshold"]] <- threshold
  }
  
  
  combinedModel[["modelpars"]] <- trainOutput[c("train.data" , "learner")]
  
  combinedModel[["test.data"]] <- test.data
  
  combinedModel[["positiveClass"]] <- positveClass
  
  # Setting a new class for the combined Model in order to predict it with a new function
  class(combinedModel) <- "WrappedCombiModel"
  
  return(combinedModel)
  
}

predict.WrappedCombiModel <- function(combinedModel , newdata , NAtoZero = T){
  
  #checking if all features are calculated or not, if all features are aviable the prediction is done based on the data otherwise the featrues are calculated
  if(! all( combinedModel$model$features %in% colnames(newdata) ) && !is.null(combinedModel[["funList"]])){
    
    newdata <- evaluateFunList(funList = combinedModel$funList , data = newdata)
    
    
  }
  
  # checking if the new data contains now all of the required features
  if(! all( combinedModel$model$features %in% colnames(newdata) )){
    stop("Erorr: The features which could be calculated from the functions stored in the WrappedCombiModel do not match the features used to train the model")
  }
  
  #nessesary to adjust the factor levels to those used for the training, grep is nessesary to discard the training dataset
  
  for(n in grep(a$model$task.desc$target , names(a$model$factor.levels) , invert = T , value = T)){

    newdata[,n] <- factor(x = newdata[,n] , levels = combinedModel$model$factor.levels[[n]])
    
  }

  
  newdata <- convertClass(newdata)
  
  
  #prediction done with the underlaying predict function
  
  prediction <- predict(combinedModel$model , newdata = newdata)
  
  #if threshold is unequal to NULL the threshold is set to the prediction
  
  if(! is.null(combinedModel$threshold)){
    
    prediction <- mlr::setThreshold(pred = prediction , threshold = combinedModel$threshold)
    
  }
  
  #assignin all predictions that contains NA to prob.True = 0
  if(NAtoZero){
    
    idx <- is.na(prediction$data$prob.FALSE)
    
    prediction$data[idx , "prob.FALSE"] <- 1
    prediction$data[idx , "prob.TRUE"] <- 0
    #TODO: think about this what happens if others than true or false are returned
    prediction$data[idx , "response"] <- FALSE
    
    
  }
  
  
  
  return(prediction)
  
  
  
  
}


retrain <- function(combinedModel , newdata , estimatingThreshold = F , tprThreshold = 0.995 , keepData = F){
  
  if(! all( combinedModel$model$features %in% colnames(newdata) ) && !is.null(combinedModel[["funList"]])){
    
    newdata <- evaluateFunList(funList = combinedModel$funList , data = newdata)
    
    
  }
  
  # checking if the new data contains now all of the required features
  if(! all( combinedModel$model$features %in% colnames(newdata) )){
    stop("Erorr: The features which could be calculated from the functions stored in the WrappedCombiModel do not match the features used to train the model")
  }
  
 
  
  ntrain <- sample(1:nrow(newdata) , 0.8*nrow(newdata))
  
  #for test data the features are not subseted
  ntest <- setdiff(1:nrow(newdata) , ntrain)
  

  
  if(keepData){
  
  combinedModel$test.data <- rbind(combinedModel$test.data , newdata[ntest , names(combinedModel$test.data) ])
  
  combinedModel$modelpars$train.data <- rbind(combinedModel$modelpars$train.data , newdata[ntrain , names(combinedModel$modelpars$train.data)])
  
  }else{
    
    combinedModel$test.data <-  newdata[ntest , names(combinedModel$test.data) ]
    
    combinedModel$modelpars$train.data <-  newdata[ntrain , names(combinedModel$modelpars$train.data) ]
    
    
    
  }
  
  newdata <- combinedModel$modelpars$train.data[,c(combinedModel$model$features , combinedModel$model$task.desc$target)]
  
  newdata <- removeNAs( data = newdata )
  
  newdata <- convertClass(newdata)
  
  task <- makeClassifTask(id = combinedModel$model$task.desc$id , data =  newdata , target = combinedModel$model$task.desc$target , positive = combinedModel$model$task.desc$positive)
  
  
  combinedModel$model <- mlr::train(learner = combinedModel$modelpars$learner , task = task)
  
  if(estimatingThreshold){
    
    resamp <- mlr::makeResampleDesc("CV" , iters = 10 ,stratify = F , predict = "both")
    
    # performing the resampling
    resampResult <- mlr::resample(combinedModel$modelpars$learner , task , resamp , measure = list(tpr , fpr , acc , ppv , auc) , models = T)
    
    # calculating the Threshold based on the prediction of the 10 fold CV
    combinedModel[["threshold"]] <- getThresholdCV(resampleResult = resampResult , train.data = combinedModel$modelpars$train.data , tprThreshold = tprThreshold)
    
    
  }
  
  
  return(combinedModel)
  
}



plotTargetDensity <- function(predictionLFQ , thresholdLFQ , lowConfidenceTargets = NULL){
  
  data <- predictionLFQ$data 

  dataline <- data.frame(Intensity_type = "LFQ" , threshold = thresholdLFQ )
  
  data$truth <- ifelse(data$truth == TRUE , "TRUE" , "FALSE")
  
  if(!is.null(lowConfidenceTargets)){
    data$truth[rownames(data) %in% lowConfidenceTargets$x] <-  "low confidence"
  }
  plot <- ggplot(data , aes(x = truth  , y = prob.TRUE, group = truth , color = truth , fill = truth)) + geom_violin(alpha = 0.5 , scale = "width") + ylab("target probability") + labs(color = "Target class" , fill = "Target class") + xlab(" ") + geom_hline( data = dataline , aes(yintercept =threshold) )
  
  plot
  
}




beanPlotFeatures <- function(data , target , subsetvariable = NULL , col = c("orange" , "blue") ,quantileCut = F  , interquantile = 10 ,  ...){
  
  idx <- vector()
  
  for(n in names(data))
  {
    idx[n]<- class(data[,n]) == "numeric" || class(data[,n]) == "integer"
  }
  
  features <- names(data)[idx]
  
  
  
  for(nfeature in features)
  {
    
    if(quantileCut){
      
      ylimLower <- -interquantile*(fivenum(data[,nfeature] , na.rm = T)[4]-fivenum(data[,nfeature] , na.rm = T)[2])
      ylimUpper <- interquantile*(fivenum(data[,nfeature] , na.rm = T)[4]-fivenum(data[,nfeature] , na.rm = T)[2])
    }else{
      ylimLower <- NULL
      ylimUpper <- NULL
      
    }
    
    
    tmp <- split(data[,nfeature] , data[,c(target , subsetvariable) ])
    
    if(length(tmp) == 2){
      par(mar=c(5.1,4.1,4.1,2.1))
      beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , side = "both" ,  col = list(col[1], c(col[2], "white")) , show.names = F , ylim = c(ylimLower , ylimUpper) )
      legend("topright" , col = col , legend = names(tmp) , pch = 19  , bty = "n")
      
      
      
    }else if(length(unique(data$target)) == 2){
      par(mar=c(11.1,4.1,4.1,2.1))
      
      beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , side = "both" ,  col = list(col[1], c(col[2], "white")) , ... , ylim = c(ylimLower , ylimUpper) )
      legend("topright" , col = col , legend = unique(data$target) , pch = 19  , bty = "n")
      par(mar=c(5.1,4.1,4.1,2.1))
      
    }else{
      
      beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , ...  , ylim = c(ylimLower , ylimUpper))
      
      
      
    }
    
    
    
  }
  
  
}

require(BBmisc)
require(RANN)
#it was added all with prob.TRUE on 27.2

nearestNeighbors <- function(uniqueIdentifier , searchspace , newData , nNeighbor = 1000  , targetColumn = "Target" )
{
  # searchspace contains only columns of intrest
  
  # newdata needs to contain all columns which are also present in the searchspace
  
  newData <- newData[ , grep(paste0(paste0("^" , colnames(searchspace) , "$") , collapse = "|") , names(newData))]
  
  if(dim(newData)[2] != dim(searchspace)[2]){
    
    stop("Search space and new data do not contain the same column.")
  }
  
  newData <- newData[,colnames(searchspace)]
  
  lengthSearchspace <- dim(searchspace)[1]
  
  searchspace <- rbind(searchspace , newData)
  
  targets <- searchspace[ , targetColumn]
  
  searchdf <- normalize(x = searchspace[,grep(targetColumn , names(searchspace) , invert = T)] , method = "standardize" )
  
  
  searchspace <- cbind(searchspace , Targets = targets)
  
  
  nearestNeighbor <- nn2(data = searchdf[,grep(targetColumn , names(searchdf) , invert = T)] , query = searchdf[-c(1:lengthSearchspace) , grep(targetColumn , names(searchdf) , invert = T)] , k = nNeighbor)
  
  neighborsdata <- apply(nearestNeighbor[["nn.idx"]] , 1 , function(x , searchspace){
    
    tmpData <- searchspace[x,]
    
    # minus one nessesary due to the fact that the first nearest neighbor is the observation itself and should not occur
    # later on the one is added to the index again
    indexObserved <- tmpData[-1 , "Targets"] == TRUE
    
    obsTrue <- min(which(indexObserved == TRUE))
    
    obsFalse <- min(which(indexObserved == FALSE) , na.rm = T)
    
    title <- c("nearest neighbor: TRUE" , "Selected observation" , "nearest neighbor: FALSE")
    
    return(cbind(tmpData[c(obsTrue+1 ,1 , obsFalse+1), ] , title = title))
    
    
    
  } , searchspace = searchspace)
  
  
  return(neighborsdata)
  
  
}



predictionNames <- function(prediction , class = c("TP" , "TN" , "FP" , "FN")){
  
  
  if(class == "TP"){
    
    name <- rownames(prediction$data)[prediction$data$truth == TRUE & prediction$data$response == TRUE] 
    
  }else if(class == "TN"){
    
    name <- rownames(prediction$data)[prediction$data$truth == FALSE & prediction$data$response == FALSE] 
    
  }else if(class == "FP"){
    
    name <- rownames(prediction$data)[prediction$data$truth == FALSE & prediction$data$response == TRUE] 
    
  }else if(class == "FN"){
    
    name <- rownames(prediction$data)[prediction$data$truth == TRUE & prediction$data$response == FALSE] 
    
  }
  
  return(name)
  
}
