


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
      beanplot::beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , side = "both" ,  col = list(col[1], c(col[2], "white")) , show.names = F , ylim = c(ylimLower , ylimUpper) )
      legend("topright" , col = col , legend = names(tmp) , pch = 19  , bty = "n",  cex = 1.3)
      
      
      
    }else if(length(unique(data$target)) == 2){
      par(mar=c(11.1,4.1,4.1,2.1))
      
      beanplot::beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , side = "both" ,  col = list(col[1], c(col[2], "white")) , ... , ylim = c(ylimLower , ylimUpper) )
      legend("topright" , col = col , legend = unique(data$target) , pch = 19  , bty = "n" , cex = 1.3)
      par(mar=c(5.1,4.1,4.1,2.1))
      
    }else{
      
      beanplot::beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , ...  , ylim = c(ylimLower , ylimUpper))
      
      
      
    }
    
    
    
  }
  
  
}



#it was added all with prob.TRUE on 27.2

nearestNeighbors <- function(searchspace , newData , nNeighbor = 1000  , targetColumn = "Target" )
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
  
  searchdf <- BBmisc::normalize(x = searchspace[,grep(targetColumn , names(searchspace) , invert = T)] , method = "standardize" )
  
  nearestNeighbor <- RANN::nn2(data = searchdf[-(lengthSearchspace+1) , ] , query = searchdf[(lengthSearchspace+1) , ] , k = nNeighbor)
  
    tmpData <- searchspace[nearestNeighbor[["nn.idx"]] , ]
    
    indexObserved <- tmpData[,targetColumn] == TRUE
    

    # minus one nessesary due to the fact that the first nearest neighbor is the observation itself and should not occur
    # later on the one is added to the index again
    obsTrue <- min(which(indexObserved == TRUE), na.rm = T)
    
    obsFalse <- min(which(indexObserved == FALSE) , na.rm = T)
    
    title <- c("nearest neighbor: TRUE" , "nearest neighbor: FALSE")
    
    neighborsdata <- cbind(tmpData[c(obsTrue , obsFalse),], title = title)
    neighborsdata <- rbind(neighborsdata , cbind(newData , title = "Selected observation"))
    

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



summarizeModel <- function(combinedModel){
  
  dataSummary <- t(as.data.frame(unlist(getHyperPars(combinedModel$modelpars$learner))))
  
  dataSummary <- cbind(dataSummary , 'number of train.data' = combinedModel$model$task.desc$size)
  

  return(dataSummary)
  
}

evaluateFunList <- function(funList , data){
  
  #For every entrie in funList call the function on data
  for(i in 1:length(funList)){
    
    # mainly nessesary in case the user uploads a self defined function
    data <- try(funList[[i]](data) , silent = T)
    
  }
  
  return(data)
  
} 
