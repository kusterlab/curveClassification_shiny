
require(shinyjs)
require(shiny)
require(shinydashboard)
require(plotly)
require(BBmisc)
require(ggplot2)
require(mlr)
require(GGally)
require(beanplot)
require(data.table)
require(RANN)
require(DT)
require(caret)
require(plotrix)
require(CurveClassification)



source("./configuration.R")

shinyServer(function(input, output , session) {
  
  ##### Definition of global variables for the app
  
  data <- reactiveValues(data = NULL , newFeatures = NULL , model = NULL , newdata = NULL , pred = NULL , evaluated = NULL , modelretrained = NULL)
  
  fgf.List <- NULL
  
  tmp.fgf.List <- NULL
  
  plotfun_Env <- new.env()
  
  plotfun <- NULL
  
  data.prediction.download <- NULL
  
  manThreshold <- NULL
  
  tmpData <- reactiveValues(GenerateModel = NULL , OptimizeModel = NULL)
  
  useShinyjs()
  ##### data import #####
  
  
  output$import.ui <- renderUI({
    
    sidebarMenu(
      
      fileInput("import.csv", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv") , multiple = T),
      
      checkboxInput("import.header", "Header", TRUE),
      
      selectInput("import.sep", "Separator", selected = ",",
                  
                  choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
      
      selectInput("import.quote", "Quote", selected = '"',
                  
                  choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))
      
    )
    
    
  })
  
  
  # processing of the uploaded dataset
  
  observe({ 
    
    f <- input$import.csv$datapath
    
    if (is.null(f)) {
      
      data$data <- NULL
      
    } else {
      
      data$data <- try(as.data.frame(data.table::rbindlist(lapply(f, read.csv, header = input$import.header, sep = input$import.sep,
                                                                  quote = input$import.quote) , use.names = TRUE, fill = TRUE)))
      
      #TO CHECK IF it works
      fgf.List <- NULL
      
      tmp.fgf.List <- NULL
      
      
      
    }
    
  })
  
  
  
  # visualization of the uploaded dataset
  
  output$import.preview <- DT::renderDataTable({
    if(!is.null(data$data)){
      #To ensure that the data is already not NULL
      req(data$data)
      d <- data$data
      #providing columnNames
      colnames(d) <- make.names(colnames(d))
      #nessesary to return the dataframe
      return(d)
    }
  }, options = list(rownames= FALSE),
  caption = "You imported the following data set")
  
  
  
  
  ##### feature generation #####
  
  output$fgf.ui <- renderUI({
    
    #Nessesary to reavalute all after the R script is sourced
    input$fgf.R$datapath
    
    sidebarMenu(
      
      fileInput("fgf.R", "Choose R script",
                accept = c(".R")),
      
      
      selectInput("fgf.function", "Feature generation function", selected = NULL,
                  
                  choices = grep("^fgf_" , ls(envir = globalenv() ) , value = T) , multiple = T , selectize = T),
      
      textInput("fgf.function.Pattern" , "Feature generation function patterns" , placeholder = "e.g. Normalized.LFQ.intensity.;Normalized.Intensity."),
      
      checkboxInput("fgf.function.paired" , "Function and patterns paired?" , value = F),
      
      actionButton("fgf.start" , label = "Generate features"),
      
      br(),
      actionButton("fgf.replace" , label = "Use data with new features") ,
      br()
      
      
      
      
      
      
    )
    
  })
  
  output$fgf.PlotControllUi <- renderUI({
    
    wellPanel(
      
      h4("Plot options"),
      
      selectInput("fgf.TargetColumn" , label = "Select target" , choices = colnames(data$data) , multiple = F , selected = TargetColumn.fgf() , selectize = T),
      
      selectInput("fgf.PositiveClass" , label = "Select positive class" , choices = unique(data$data[,isolate(TargetColumn.fgf())]) , multiple = F , selected = PositiveClass.fgf() , selectize = T)
      
      
    )
    
  })
  
  
  observe( {
    data$newFeatures
    if(is.null(data$newFeatures)){
      
      shinyjs::disable("fgf.TargetColumn")
      shinyjs::disable("fgf.PositiveClass")
      
    }else{
      shinyjs::enable("fgf.TargetColumn")
      shinyjs::enable("fgf.PositiveClass")
      
    }
    
    
  }
  )
  
  
  # Selection of the target variable for data visualization
  
  TargetColumn.fgf <- reactive({
    
    if(is.null(input$fgf.TargetColumn) || is.null(data$newFeatures)|| is.null(data$data)){
      
      return(NULL)
      
    }else{
      
      return(input$fgf.TargetColumn)
      
    }
    
  })
  
  
  # Selection of the positive class for data visualization
  
  PositiveClass.fgf <- reactive({
    
    if(is.null(input$fgf.PositiveClass) || is.null(data$data)){
      
      return(NULL)
      
    }else{
      
      return(input$fgf.PositiveClass)
      
    }
    
  })
  
  
  # source the uploaded R-script into the global enviroment
  
  observe({
    
    f = input$fgf.R$datapath
    
    if (is.null(f)) {
      
      NULL
      
    } else {
      
      try(source(f,  local = F))
      
    }
    
  })
  
  
  # generation of the dataset with features
  #TODO: Check this part if it is working as expected ...
  observeEvent(input$fgf.start , {
    
    # wrapping into a render text enables easy output of messages if something has failed
    
    output$fgf.messages <- renderText(isolate({
      
      # selection of all patterns on which the function lists should be evaluated
      
      patternFun <- strsplit(input$fgf.function.Pattern , ";")[[1]]
      
      if(input$fgf.function.paired){
        
        # Checking if the length of functions are equally to the length of specified patterns
        
        validate(need(length(input$fgf.function) == length(patternFun) , message = "If 'functions and patterns paired?' is selected the length of functions needs to be the same as the length of patterns!\n"))
        
      }else{
        
        # Checking whether the length of pattern is one since the pattern are not paired
        
        validate(need(length(patternFun) == 1, message = "If 'functions and patterns paired?' is deselected the length of patterns has to be one!\n") )
        
        # Extending the input pattern to the same length, as the function pattern
        
        patternFun <- rep(input$fgf.function.Pattern , times = length(input$fgf.function))
        
      }
      
      # Checking for the dataset for the patterns, and generate the functionlist
      
      if(!is.null(input$fgf.function) && is.null(tmp.fgf.List)){
        
        validate(need({length(grep(paste0(patternFun , collapse = "|") , names(data$data))) > 0}, message = "The specified pattern does not occur in the dataset!\n"))
        
        tmp <- CurveClassification::generateFunctionList(input$fgf.function , patternFun)
        
        
        # Checking for the dataset for the patterns, and combine it with the existing fgf_list 
        #TODO: Check whether this causes the duplicates
      }else if(!is.null(input$fgf.function)){
        
        validate(need({length(grep(paste0(patternFun , collapse = "|") , names(data$data))) > 0}, message = "The specified pattern does not occur in the dataset!\n"))
        
        tmp <- CurveClassification::generateFunctionList(input$fgf.function , patternFun)
        
        tmp <- c(tmp.fgf.List , tmp)
        
        tmp <- tmp[unique(names(tmp))]
        
      }
      
      # Calculation of the features for the new data
      validate(need(!is.null(tmp), message = "No feature generation functions selected!\n"),
               
               need(!is.null(data$data) , message = "No data set selected!\n"))
      
      # packages needs to be applied and installed in the uploaded script, furthermore it does not work if once an error occured.
      # length(fgf.List):length(tmp.fgf.List) nessesary to avoid duplications in the column
      d <- try(CurveClassification:::evaluateFunList(tmp , data$data) , silent = F)
      
      #TODO: CHECK why this shit is not working
      
      if(class(d) != "try-error"){
        
        tmp.fgf.List <<- tmp
        
        
      }
      validate(need(class(d) != "try-error" , message = "An error occured during feature calculation no features calculated!\n"))
      
      
      
      data$newFeatures <<- d
      
      return(NULL)
      
      
    }))
  })
  
  
  # overwrites the uploaded data with the data from feature generation function
  
  observeEvent(input$fgf.replace , {
    
    req(!is.null(data$newFeatures))
    
    #Ensure that only unique names and functions are there. So duplicates should be  avoided.
    data$data <<- data$newFeatures[,unique(colnames(data$newFeatures))]
    tmp <- c(fgf.List , tmp.fgf.List)
    
    fgf.List <<- tmp[unique(names(tmp))]
    
    data$newFeatures <<- NULL
    tmp.fgf.List <<- NULL
    
    
  })
  
  
  # visualization of the summary for every variable
  
  output$fgf.preview <- DT::renderDataTable({
    
    
    if(!is.null(data$newFeatures)){
      #To ensure that the data is already not NULL
      req(data$data)
      d <- data$newFeatures
      #providing columnNames
      d <- summarizeColumns(d)
      colnames(d) <- make.names(colnames(d))
      #nessesary to return the dataframe
      d
    }
  },  selection = 'single',
  caption = "You generated the following new dataset" )
  
  
  # facing the two distribution for positive and negative class of the selected feature
  output$fgf.beanplot <- renderPlot({
    
    validate(need(!is.null(PositiveClass.fgf()) , message = "In order to visualize the features a positive class needs to be selected!\n") ,
             need(!is.null(TargetColumn.fgf()) , message = "In order to visualize the features a target variable needs to be selected!\n"))#,
    
    #selected features
    selectedRows <- input$fgf.preview_rows_selected
    
    
    d <- data$newFeatures
    
    # replacing the entries with the target columns temporarly with positive and negative
    
    d[ , TargetColumn.fgf() ] <- ifelse(d[ , TargetColumn.fgf() ] == PositiveClass.fgf() , "positive" , "negative" )
    
    
    beanPlotFeatures(data = d[, c( TargetColumn.fgf() , colnames( data$newFeatures )[ selectedRows ] )] , target = TargetColumn.fgf())
    
    
  })
  
  ##### generate new models #####
  
  
  features <- reactive({
    
    if(is.null(input$newModel.features)){
      
      return(NULL)
      
    }else{
      
      return(input$newModel.features)
      
    }
    
  })
  
  TargetColumn <- reactive({
    
    #TODO: Bug is fixed but there is maybe a better way to realize this
    if (is.null(input$newModel.TargetColumn) || is.null(data$data) || ( !is.null(input$newModel.TargetColumn) && input$newModel.TargetColumn == "" ) ) {
      
      return(NULL)
      
      
    }else{
      toggleVars$TargetColumn <<- T
      return(input$newModel.TargetColumn)
      
    }
    
  })
  
  toggleVars <- reactiveValues(PositiveClass = T , TargetColumn = T )
  PositiveClass <- reactive({
    
    if(is.null(input$newModel.PositiveClass) || is.null(data$data)){
      
      return(NULL)
      
    }else{
      toggleVars$PositiveClass <<- T
      return(input$newModel.PositiveClass)
     
    }
    
  })
  
  tuneThreshold <- reactive({
    
    if(is.null(input$newModel.tuneThreshold)){
      
      return(FALSE)
      
    }else{
      
      return(input$newModel.tuneThreshold)
      
    }
    
  })
  
  usRate <- reactive({
    if(!is.null(PositiveClass()) && !is.null(TargetColumn()) && (is.null(input$newModel.usRate) || toggleVars$PositiveClass || toggleVars$TargetColumn)){
      ntotal <- dim(data$data)[1]
      
      npositive <- sum(data$data[,input$newModel.TargetColumn] == input$newModel.PositiveClass)
      
      if(ntotal > 3*npositive){
        
        fac <-npositive/ntotal
        
        toggleVars$PositiveClass <<- F
        toggleVars$TargetColumn <<- F
        return(round(3*fac , digits = 4))
        
      }else{
        return(1)
      }
    }else if(is.null(PositiveClass()) || is.null(TargetColumn())|| is.null(input$newModel.usRate)){
      
      return(0.05)
      
    }else{
      
      return(input$newModel.usRate)
    } 
  
  })
  
  splitDataNewMod <- reactive({
    
    
    if(is.null(input$newModel.splitData)){
      
      return(0.8)
      
    }else{
      
      return(input$newModel.splitData)
      
    }
    
  })
  
  
  output$newModel.ui <- renderUI({
    
    sidebarMenu(
      
      selectInput("newModel.features" , label = "Exclude features" , choices = colnames(data$data) , multiple = T , selected = isolate(features()) , selectize = T) ,
      
      selectInput("newModel.TargetColumn" , label = "Select target" , choices = colnames(data$data) , multiple = F , selected = TargetColumn() , selectize = T),
      #TODO: Check for a better solution for the bug occuring for the reason of undefined columns selected
      selectInput("newModel.PositiveClass" , label = "Select positive class" , choices = unique(data$data[,isolate(TargetColumn())]) , multiple = F , selected = PositiveClass() , selectize = T),
      
      div(style = "float:right;" ,  actionLink("newModel.advancedSettings" , label = "Advanced settings")),
      br(),
      
      
      checkboxInput("newModel.tuneThreshold" , label = "Tune threshold?" , value = F),
      # TODO: Better label than "Tpr tune value"
      shinyjs::hidden(sliderInput("newModel.tprTuneValue" , label = "Tpr tune value" , min = 0 , max = 0.999 ,value = 0.995 ,  step = 10^-3 , width = "95%")),
      
      actionButton("newModel.train" , label = "train"),
      
      
      downloadButton("saveModel" , "Download full model"),
      
      downloadButton("saveModelPipeline" , "Download model for pipeline")
      
    )
    
    
  })
  
  observeEvent(input$newModel.advancedSettings, {
    showModal(modalDialog(
      title = "Advanced settings",
      list(
        numericInput("newModel.splitData" , label = "Ratio to split data into train and test" , min = 0.05 , max = 0.999 , step = 0.05 , value = isolate(splitDataNewMod())),
        
        if(!is.null(data$data)){
          
          paste("negative observations : positive observations  ;" ,round(dim(data$data)[1]/sum(data$data[,input$newModel.TargetColumn] == input$newModel.PositiveClass) , digits = 1) , ": 1"   , sep = " ")
        },
        
        br(),
        br(),
        
        numericInput("newModel.usRate" , label = "Select a undersampling rate" , value = usRate() , min = 10^-4 , max = 1 , step = 10^-4)
        
        
      ), footer = modalButton("Confirm")
    ))
  })
  
  
  # Show and hide the selector of tprTuneValue
  
  observe({

    if(!is.null(input$newModel.tuneThreshold) && input$newModel.tuneThreshold){
      
      shinyjs::show("newModel.tprTuneValue" , animType = "fade")
      
    }else{
      
      shinyjs::hide("newModel.tprTuneValue" , animType = "fade")
      
    }
    
  })
  
  
  
  output$saveModel <- downloadHandler(filename = function(){"model.RData"} , content = function(file){
    
    # integration of the current plotfunction into the model
    
    data$model[["plotfun_Env"]] <- plotfun_Env
    
    saveRDS(data$model , file = file)
    
  })
  
  output$saveModelPipeline <- downloadHandler(filename = function(){"model_for_pipeline.RData"} , content = function(file){
    
    # integration of the current plotfunction into the model
    
    data$model[["plotfun_Env"]] <- plotfun_Env
    
    tmp <- data$model
    
    tmp$data <- NULL
    
    saveRDS(tmp , file = file)
    
  })
  
  # training of the model
  
  observeEvent(input$newModel.train , {
    
    # wrapping into a render text enables easy output of messages if something has failed
    
    output$newModelMessages <- renderText({
      
   
      isolate({
        
        txt <- NULL
 
        if(is.null(usRate()) || usRate() == 0){
          
          txt <- c(txt , "The selected positive class does not contain a single observations.\n")
          
        }
        
        if(is.null(data$data)){
          
          
          txt <- c(txt , "Please select a dataset.\n")
          
          
        }
        
        if(dim(data$data)[2] < 2 ){
          
          txt <- c(txt ,"The uploaded data needs to contain at least two columns")
        }
        
        
        if(length(txt)>0){
          
          return(paste("<font color=\"#FF0000\"><b>", txt, "</b></font>"))
        }
        
        if(usRate() != 0){
          
          #TODO: is this isolate still necessary
          
          isolate({
            
            # random assignment of train data and test data
            n <- sample(1:dim(data$data)[1] , splitDataNewMod()*dim(data$data)[1])
            
            d <- data$data
            
            # for training exchange the values of the targetcolumn into the two classes TRUE and FALSE
            
            d[,TargetColumn()] <- ifelse(d[,TargetColumn()] == PositiveClass() , TRUE , FALSE)
            
            
            data_train <- d[ n , ]
            
            data_test <- d[ setdiff( 1:dim(data$data)[1] , n) , ]
            
            usedFeatures <- grep(paste0(paste0("^" , c(features() , TargetColumn()), "$") , collapse = "|") , names(d) , invert = T , value = T)
            
            data$model <- CurveClassification::generateModel(classifier = classifier , us.rate = usRate() , features = usedFeatures , data = data_train , targetVariable = TargetColumn() , positiveClass = "TRUE" , estimatingThreshold = tuneThreshold() , tprThreshold = input$newModel.tprTuneValue )
            
            
            data$model <- CurveClassification::combineModel(trainOutput = data$model , featureFunctionList = fgf.List , test.data = data_test , positveClass = PositiveClass())
            
            
            return(paste("<font color=\"#7CFC00\"><b>", "Model trained sucessfull", "</b></font>"))
          })
          
          
        }
        
      })
      
    })
    
  })
  
  
  
  # Prediction of the training and test data in case the model is sucessfully trained
  observe({
    
    validate(need(!is.null(data$model) , "No model avaiable!\n"),
             need(!is.null(data$model$data) , "No data for the model available!"))
    
    isolate({
      pred <- try(predict(data$model , newdata = data$model$data , NAtoZero = T))
      
      if(!is.null(data$model$threshold)){
        
        pred <- setThreshold(pred , data$model$threshold)
      }
      
      validate(need(class(pred) != "try-error" , "No prediction possible, please check if the features are similar to the training data!\n"))
      
      namesToExtract <- grep("prob.FALSE" , names(pred$data) , invert = T)
      
      d <- pred$data[,namesToExtract]
      
      
      colnames(d) <- gsub("prob.TRUE" , "probability" , colnames(d))
      colnames(d) <- gsub("response" , "prediction" , colnames(d))
      
      #removal of stored Target column nessesary to avoid to columns with the same name
      
      tmpData$GenerateModel <<- cbind(d , data$model$data[,grep(paste0("^" ,pred$task.desc$target,"$" ) , names(data$model$data) , invert = T) ] )
      
    })
    
  })   
  
  
  output$generateModelData <-  DT::renderDataTable({
    
    return(DT::datatable(data = tmpData$GenerateModel , filter = 'top' , options = list(rownames= FALSE)))
    
    
  }, caption = "Data base of model")
  
  
  output$NNgenerateModelData <-  DT::renderDataTable({
    
    return(DT::datatable(data = tmpData$GenerateModel , filter = 'top' , options = list(rownames= FALSE), selection = 'single'))
    
    
  }, caption = "Select an observation to find it's nearest neighbors")
  
  
  
  # Plotting selected observations
  
  observe({
    
    d <- tmpData$GenerateModel[input$generateModelData_rows_selected , ]
    
    
    output$plotsGenerateModels <- renderUI({
      
      validate(need(!is.null(plotfun) , message = "No plot function found!\n"))
      
      if(is.null(d) || dim(d)[1] < 1 ){
        
        return(NULL)
      }else{
        
        lapply(1:nrow(d) ,  function (x, plotfun , data) {
          
          
          output[[paste0("plotGenerateModel",x)]] <- renderPlot({
            
            temp <-  plotfun(data[x , ])
            
            
          } )
          
          plotOutput(paste0("plotGenerateModel",x))
          
        } , plotfun = plotfun , data = d)
      }
    })
    
  })
  
  
  
  # ploting the neareast neighbor for the selected observation
  
  observe({
    
    newData <- tmpData$GenerateModel[input$NNgenerateModelData_rows_selected , ]
    
    
    validate(need(!is.null(plotfun) , "No plot function avaiable!\n"),
             need(!is.null(tmpData$GenerateModel) , "No model selected!\n"),
             need(!is.null(input$NNgenerateModelData_rows_selected) , "No row selected!\n"),
             need(!is.null(data$model$data) , "No data for the model available!")
    )
    
    # removal of the selected observation from the searchspace because otherwise this would be found as the nearest neighbor
    colnames(newData) <- gsub("truth" , replacement = data$model$model$task.desc$target , x = colnames(newData)) 
    searchspace <- data$model$data[-input$NNgenerateModelData_rows_selected , c(data$model$model$features , data$model$model$task.desc$target)]
    
    searchspace <- CurveClassification:::removeNAs(searchspace)
    
    neighbours <- try(nearestNeighbors(searchspace = searchspace , newData = newData , targetColumn = data$model$model$task.desc$target , nNeighbor = nrow(searchspace)))
    
    if(class(neighbours) != "try-error"){
      
      # get with the rownames the nearest neighbors from the original data which contain everything required for plotting
      neighbours <- data$model$data[rownames(neighbours) , ]
      #neighbours contians additional the column targets which is an artefact from the neighbors function but has no effect
    }
    
    
    output$nearestNeighborTRUEGenNewMod <- renderPlot({
      
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      plotfun(neighbours[1,])
      
    })
    output$nearestNeighborExGenNewMod <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      
      plotfun(neighbours[3,])
      
      
      
    })
    
    output$nearestNeighborFALSEGenNewMod <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      
      plotfun(neighbours[2,])
      
      
      
    })
    
  })
  
  
  # Generation of the performance measures
  
  observeEvent( data$model, {
    
    validate(need(!is.null(data$model$data) , "No data for the model available!"))
    
    pred <- predict(data$model , newdata = data$model$data[ data$model$data$group == "test" , ] , NAtoZero = T)
    
    if(!is.null(data$model$threshold)){
      
      pred <- setThreshold(pred , threshold = data$model$threshold)
      
      
    }
    
    
    output$ConfusionMatrix <- renderPrint({
      calculateConfusionMatrix(pred)
    })
    
    output$PerformanceMeasures <- renderTable({
      
      perf <- performance(pred , measures = list(tpr , fpr , acc , ppv , auc))
      
      perf <- t(as.data.frame(perf))
      
      colnames(perf) <- c("True positive rate" , "False positive rate" , "Accuracy" , "Precision" , "Area under ROC curve")
      return(perf)
      
      
    } , rownames = F , digits = 4)
    
    output$performanceVsThreshold <- renderPlot({
      
      return(plotThreshVsPerf(generateThreshVsPerfData(pred , measures = list(tpr , fpr , ppv , acc)) , pretty.names = T , mark.th = pred$threshold["TRUE"]))
      
      
    })
    
    
    output$probabilityDistribution <- renderPlot({
      
      
      return( plotTargetDensity(pred , thresholdLFQ = pred$threshold["TRUE"]) )
      
      
      
    })
    
    
  })
  
  
  #### optimize model ####
  
  output$optimize.ui <- renderUI({
    
    sidebarMenu(
      
      fileInput("optimize.model" , "Choose model" , accept = ".RData"),
      
      wellPanel(
        fileInput("optimize.newdata" , "Choose reannotated data" , accept = ".csv" , multiple = T),
        
        checkboxInput("optimize.header", "Header", TRUE),
        
        selectInput("optimize.sep", "Separator", selected = ",", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"))
      ),
      
      checkboxInput("optimize.tuneThreshold" , label = "Tune threshold?" , value = F),
      
      shinyjs::hidden(sliderInput("optimize.tprTuneValue" , label = "Tpr tune value" , min = 0 , max = 0.999 ,value = 0.995 ,  step = 10^-3 , width = "95%")),
      
      checkboxInput("optimize.keepData" , label = "Keep old training and test data" , value = F),
      
      actionButton("optimize.retrain" , label = "optimize")
      
      
    )
    
    
    
  })
  
  observe({
    
    if(!is.null(input$optimize.tuneThreshold) && input$optimize.tuneThreshold){
      
      shinyjs::show("optimize.tprTuneValue" , animType = "fade")
      
    }else{
      
      shinyjs::hide("optimize.tprTuneValue" , animType = "fade")
      
    }
    
    
  })
  
  # Makes the exchange button only available if a retrained model is available  
  output$optimizeExchangeButton <- renderUI({
    
    if(!is.null(data$modelretrained)){
      
      sidebarMenu(
        actionButton("optimizeExchangeButton" , label = "Use new model"),
        
        downloadButton("optimizeSaveModel" , label = "Download full model"),
        
        downloadButton("optimizeSaveModelPipeline" , label = "Download model for pipeline")
        
      )
    }else{
      NULL
    }
    
    
  })
  
  # Exchange of the initial model with the retrained model.
  
  observeEvent(input$optimizeExchangeButton , {
    
    data$model <- data$modelretrained
    
    data$modelretrained <- NULL
    
    data$evaluated <- NULL
    
    
    
    
  })
  
  #Upload of reevaluated Data
  
  observe({
    
    f = input$optimize.newdata$datapath
    
    if (is.null(f)) {
      
      data$evaluated <- NULL
      
    } else {
      
      data$evaluated <- as.data.frame(data.table::rbindlist(lapply(f, read.csv, header = input$optimize.header, sep = input$optimize.sep) , use.names = TRUE, fill = TRUE))
      
    }
    
    
    
  })
  
  #Upload of a model that should be retrained
  
  observeEvent(input$optimize.model$datapath , {
    
    f = input$optimize.model$datapath
    
    if (!is.null(f)) {
      
      data$model <- try(readRDS(f))
      
    }
    
    if(class(data$model) == "try-error"){
      data$model <- NULL
      
    }
    
    if(!is.null(data$model[["plotfun_Env"]])){
      
      plotfun_Env <<- data$model$plotfun_Env
      
      if(length(grep("^plot_" , ls(envir = plotfun_Env))) == 1){
        
        plotfun <<- plotfun_Env[[ (grep("^plot_" , ls(envir = plotfun_Env) , value = T))]]
      }
      
    }
    
  })
  
  
  
  
  tuneThresholdOptimize <- reactive({
    
    
    if(is.null(input$optimize.tuneThreshold)){
      
      return(FALSE)
      
    }else{
      
      return(input$optimize.tuneThreshold)
      
    }
    
  })
  
  
  # Retraining of the model
  
  observeEvent(input$optimize.retrain , {
    
    
    output$optimizeMessages <- renderText({
      
      isolate({
        
        if(is.null(data$evaluated) || is.null(data$model) ){
          
          txt <- "You need to select a model and/or new annotated data.\n"
          return(paste("<font color=\"#FF0000\"><b>", txt, "</b></font>"))
        }
        
        
        d <- data$evaluated
        
        d[,data$model$model$task.desc$target] <- ifelse(d[,data$model$model$task.desc$target] == data$model$positiveClass , TRUE , FALSE)
        
        data$modelretrained <- CurveClassification:::retrain(combinedModel = data$model , newdata = d , estimatingThreshold = tuneThresholdOptimize() , tprThreshold = input$optimize.tprTuneValue , keepData = input$optimize.keepData)
        
        return(paste("<font color=\"#7CFC00\"><b>", "Model retrained sucessfull", "</b></font>"))
      })
      
    })
    
  })
  
  # Calculation and visualization of performances of the two models
  
  observeEvent(data$modelretrained , {
    
    validate(need(!is.null(data$modelretrained$data) , "No data for the model available!"))
    
    
    pred <- predict(data$model , newdata = data$modelretrained$data[data$modelretrained$data$group == "test",], NAtoZero = T)
    
    predNewMod <- predict(data$modelretrained , newdata = data$modelretrained$data[data$modelretrained$data$group == "test",], NAtoZero = T)
    
    if(!is.null(data$model$threshold)){
      
      pred <- setThreshold(pred , threshold = data$model$threshold)
      
      
    }
    if(!is.null(data$modelretrained$threshold)){
      
      predNewMod <- setThreshold(predNewMod , threshold = data$modelretrained$threshold)
      
      
    }
    
    
    output$ConfusionMatrix.old <- renderPrint({
      calculateConfusionMatrix(pred)
    } )
    
    output$PerformanceMeasures.old <- renderTable({
      
      perf <- performance(pred , measures = list(tpr , fpr , acc , ppv , auc))
      
      perf <- t(as.data.frame(perf))
      
      colnames(perf) <- c("True positive rate" , "False positive rate" , "Accuracy" , "Precision" , "Area under ROC curve")
      return(perf)
      
      
    } , rownames = F , digits = 4)
    
    output$performanceVsThreshold.old <- renderPlot({
      
      return(plotThreshVsPerf(generateThreshVsPerfData(pred , measures = list(tpr , fpr , ppv , acc)) , pretty.names = T , mark.th = pred$threshold["TRUE"]))
      
      
    })
    
    
    output$probabilityDistribution.old <- renderPlot({
      
      
      return( plotTargetDensity(pred , thresholdLFQ = pred$threshold["TRUE"]) )
      
      
    })
    
    
    output$ConfusionMatrix.new <- renderPrint({
      calculateConfusionMatrix(predNewMod)
    } )
    
    output$PerformanceMeasures.new <- renderTable({
      
      perf <- performance(predNewMod , measures = list(tpr , fpr , acc , ppv , auc))
      
      perf <- t(as.data.frame(perf))
      
      colnames(perf) <- c("True positive rate" , "False positive rate" , "Accuracy" , "Precision" , "Area under ROC curve")
      return(perf)
      
      
    } , rownames = F , digits = 4)
    
    output$performanceVsThreshold.new <- renderPlot({
      
      return(plotThreshVsPerf(generateThreshVsPerfData(predNewMod , measures = list(tpr , fpr , ppv , acc)) , pretty.names = T , mark.th = predNewMod$threshold["TRUE"]))
      
      
    })
    
    
    output$probabilityDistribution.new <- renderPlot({
      
      
      return( plotTargetDensity(predNewMod , thresholdLFQ = predNewMod$threshold["TRUE"]) )
      
    })
    
  })
  
  
  # prediction for the output of the underlaying data
  
  observe({
    
    validate(need(!is.null(data$modelretrained) , "No retrained model avaiable!\n"),
             need(!is.null(data$modelretrained$data) , "No data for the model available!"))
    
    isolate({
      pred <- try(predict(data$modelretrained , newdata = data$modelretrained$data, NAtoZero = T))
      
      if(!is.null(data$modelretrained$threshold)){
        
        pred <- setThreshold(pred , data$modelretrained$threshold)
      }
      
      validate(need(class(pred) != "try-error" , "No prediction possible, please check if the features are similar to the training data!\n"))
      
      namesToExtract <- grep("prob.FALSE" , names(pred$data) , invert = T)
      
      d <- pred$data[,namesToExtract]
      
      
      colnames(d) <- gsub("prob.TRUE" , "probability" , colnames(d))
      colnames(d) <- gsub("response" , "prediction" , colnames(d))
      
      #removal of stored Target column nessesary to avoid to columns with the same name
      tmpData$OptimizeModel <<- cbind( d , data$modelretrained$data[,grep(paste0("^" ,pred$task.desc$target,"$" ) , names(data$modelretrained$data) , invert = T) ])
      
    })
    
  })
  
  # output of the underlying model data for prediction
  
  output$optimizeNewdata <-  DT::renderDataTable({
    
    return(DT::datatable(data = tmpData$OptimizeModel  , filter = 'top',options = list(rownames= FALSE)))
    
  }, caption = "Data base of model")
  
  
  # Visualization of the selected rows
  
  observe({
    
    d <- tmpData$OptimizeModel[input$optimizeNewdata_rows_selected , ]
    
    output$plotsOptimizeModel <- renderUI({
      
      validate(need(!is.null(plotfun) , message = "No plotfunction found!\n"))
      
      if(is.null(d) || dim(d)[1] < 1 ){
        
        return(NULL)
      }else{
        
        lapply(1:nrow(d) ,  function (x, plotfun , data) {
          
          
          output[[paste0("plotOptimizeModel",x)]] <- renderPlot({
            
            temp <-  plotfun(data[x , ])
            
          } )
          
          plotOutput(paste0("plotOptimizeModel",x))
          
        } , plotfun = plotfun , data = d)
      }
    })
    
  })
  
  
  # Generation of datatable for nearest neighbor plotting
  
  output$NNoptimizeNewdata <-  DT::renderDataTable({
    
    return(DT::datatable(data = tmpData$OptimizeModel  , filter = 'top',options = list(rownames= FALSE) , selection = "single"))
    
  }, caption = "Select an observation to find the nearest neighbors")
  
  
  #Generation of nearest neighobr plots
  
  observe({
    
    newData <- tmpData$OptimizeModel[input$NNoptimizeNewdata_rows_selected , ]
    
    
    validate(need(!is.null(plotfun) , "No plot function avaiable!\n"),
             need(!is.null(tmpData$OptimizeModel) , "No model selected!\n"),
             need(!is.null(input$NNoptimizeNewdata_rows_selected) , "No row selected!\n"),
             need(!is.null(data$modelretrained$data) , "No data for the model available!")
    )
    
    colnames(newData) <- gsub("truth" , replacement = data$modelretrained$model$task.desc$target , x = colnames(newData)) 
    
    searchspace <- data$modelretrained$data[-input$NNoptimizeNewdata_rows_selected , c(data$modelretrained$model$features , data$modelretrained$model$task.desc$target)]
    
    searchspace <- CurveClassification:::removeNAs(searchspace)
    
    neighbours <- try(nearestNeighbors(searchspace = searchspace , newData = newData , targetColumn = data$modelretrained$model$task.desc$target , nNeighbor = nrow(searchspace)))
    
    if(class(neighbours) != "try-error"){
      # get with the rownames the nearest neighbors from the original data which contain everything required for plotting
      neighbours <- data$modelretrained$data[rownames(neighbours) , ]
      
    }
    
    output$nearestNeighborTRUEOptimizeMod <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      plotfun(neighbours[1,])
      
    })
    output$nearestNeighborExOptimizeMod <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      
      plotfun(neighbours[3,])
      
    })
    
    output$nearestNeighborFALSEOptimizeMod <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      
      plotfun(neighbours[2,])
      
    })
    
  })
  
  
  # Download the optimized model
  
  output$optimizeSaveModel<- downloadHandler(filename = function(){"reevaluatedModel.RData"} , content = function(file){
    
    data$modelretrained[["plotfun_Env"]] <- plotfun_Env
    
    
    saveRDS(data$modelretrained , file = file)
    
  })
  
  # Download the optimized model for pipeline integration
  
  output$optimizeSaveModelPipeline<- downloadHandler(filename = function(){"reevaluatedModel_for_pipeline.RData"} , content = function(file){
    
    data$modelretrained[["plotfun_Env"]] <- plotfun_Env
    
    tmp <- data$modelretrained
    
    tmp$data <- NULL
    
    
    saveRDS(tmp , file = file)
    
  })
  
  ####Validate Model ####
  
  output$validate.ui <- renderUI({
    sidebarMenu(
      
      radioButtons("validate.modelSelection" , label = "Select a model" , choices = c("model" , "optimized model") , selected = "model" ),
      br(),
      
      checkboxInput("validate.allData" , label = "only for test data" , value = T),
      br(),
      
      actionButton("validate.go" , label = "Generate plots")
      
    )
    
  })
  
  #Generate plots of all false negatives
  
  observeEvent(input$validate.go , {
    
    output$plotsFN <- renderUI({
      isolate({
        if(input$validate.modelSelection == "optimized model"){
          
          tmpModel <- data$modelretrained
          
        }else if(!is.null(input$validate.modelSelection)){
          
          tmpModel <- data$model
          
        }else{
          tmpModel <- NULL
        }
        
        validate(need(!is.null(tmpModel) , "The selected model is not available!\n"),
                 need(!is.null(plotfun) , "No plot function available!\n"))
        
        
        if(input$validate.allData){
          
          pred <- predict(tmpModel , newdata = tmpModel$data[tmpModel$data$group == "test", ], NAtoZero = T)
          
          
        }else{
          pred <- predict(tmpModel , newdata = tmpModel$data, NAtoZero = T)
          
        }
        
        
        names <- predictionNames(pred , "FN")
        
        d <- tmpModel$data[names , ]
        
        if(nrow(d) == 0){
          return(NULL)
        }
        
        
        
        lapply(1:nrow(d) ,  function (x, plotfun , data) {
          
          
          output[[paste0("plotFN",x)]] <- renderPlot({
            
            temp <-  plotfun(data[x , ])
            
          } )
          
          plotOutput(paste0("plotFN",x))
          
        } , plotfun = plotfun , data = d)
        
      })
    })
  })
  
  
  #Generate plots of all false positives
  
  observeEvent(input$validate.go , {
    
    output$plotsFP <- renderUI({
      isolate({
        if(!is.null(input$validate.modelSelection) && input$validate.modelSelection == "optimized model"){
          
          tmpModel <- data$modelretrained
          
        }else if(!is.null(input$validate.modelSelection)){
          
          tmpModel <- data$model
          
        }else{
          tmpModel <- NULL
        }
        
        validate(need(!is.null(tmpModel) , "The selected model is not available!\n"),
                 need(!is.null(plotfun) , "No plot function available!\n"))
        
        
        if(input$validate.allData){
          #Bug if the new and the old model should be compared since the data stored in the model is used
          pred <- predict(tmpModel , newdata = tmpModel$data[tmpModel$data$group == "test" , ], NAtoZero = T)
          
          
        }else{
          
          pred <- predict(tmpModel , newdata = tmpModel$data, NAtoZero = T)
        }
        
        names <- predictionNames(pred , "FP")
        
        d <- tmpModel$data[names , ]
        
        if(nrow(d) == 0){
          return(NULL)
        }
        
        
        lapply(1:nrow(d) ,  function (x, plotfun , data) {
          
          
          output[[paste0("plotFP",x)]] <- renderPlot({
            
            temp <-  plotfun(data[x , ])
            
          } )
          
          plotOutput(paste0("plotFP",x))
          
        } , plotfun = plotfun , data = d)
        
      })
    })
  })
  
  #### predict #####
  
  output$predict.ui <- renderUI({
    
    sidebarMenu(
      
      fileInput("predict.model" , "Choose model" , accept = ".RData"),
      
      wellPanel(
        
        fileInput("predict.csv", "Choose CSV File",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv") , multiple = T),
        
        checkboxInput("predict.header", "Header", TRUE),
        
        selectInput("predict.sep", "Separator", selected = ",", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"))
      ),
      
      br(),
      
      #TODO: Think about the output as it is implementet now
      checkboxInput("predict.NAs" , label = "Force NA containing observations to probability of 0?" , value = F),
      
      checkboxInput("predict.specifyThreshold" , label = "Set manual threshold" , value = F),
      
      numericInput("predict.manualThreshold" , label = "Choose threshold" , min = 0  , value = 0.5, max = 1 , step = 10^-2),
      
      shinyjs::disabled(actionButton("predict.go" , "Predict")),
      
      shinyjs::disabled(downloadButton("predict.Download" , label = "Download .csv"))
      
    )
    
  })
  
  observe({
    
    if(!is.null(data$newdata) && !is.null(data$model)){
      
      shinyjs::enable( id = "predict.go" )
      
      shinyjs::enable( id = "predict.Download")
      
    }else{
      
      shinyjs::disable( id = "predict.go")
      
      shinyjs::disable( id = "predict.Download")
      
    }
    
  })
  
  
  # storing the manuel threshold if selected
  
  observe({
    
    if(!is.null(input$predict.specifyThreshold) && input$predict.specifyThreshold){
      
      manThreshold <<- input$predict.manualThreshold
      
    }else{
      
      manThreshold <<- NULL
    }
    
  })
  
  
  # upload of a dataset which is to be predicted predict
  
  observe({
    
    f = input$predict.csv$datapath
    
    if (is.null(f)) {
      
      data$newdata <- NULL
      
    } else {
      
      data$newdata <- as.data.frame(data.table::rbindlist(lapply(f, read.csv, header = input$predict.header, sep = input$predict.sep) , use.names = TRUE, fill = TRUE))
      
    }
    
  })
  
  # upload of a model to be predicted
  
  observeEvent(input$predict.model$datapath , {
    
    f = input$predict.model$datapath
    
    if (!is.null(f)) {
      
      data$model <- try(readRDS(f))
      
      
    }
    if(class(data$model) == "try-error"){
      data$model <- NULL
      
    }
    
    if(!is.null(data$model[["plotfun_Env"]])){
      
      plotfun_Env <<- data$model$plotfun_Env
      
      if(length(grep("^plot_" , ls(envir = plotfun_Env))) == 1){
        
        plotfun <<- plotfun_Env[[ (grep("^plot_" , ls(envir = plotfun_Env) , value = T))]]
      }
      
    }
    
  })
  
  
  observeEvent(input$predict.specifyThreshold, {
    
    if (input$predict.specifyThreshold){
      shinyjs::show(id = "predict.manualThreshold", animType = "fade")
    }else{
      shinyjs::hide(id = "predict.manualThreshold",animType = "fade")
    }
  })
  
  
  
  observeEvent(input$predict.go ,{
    
    data$pred <- try(predict(data$model , newdata = data$newdata , NAtoZero = input$predict.NAs ))
    
    if(class(data$pred)[1] != "try-error" && input$predict.specifyThreshold){
      
      data$pred <- setThreshold(data$pred , ifelse(is.null(manThreshold) , 0.5 , manThreshold))
      
    }
    
  })
  
  # Visualisation of the prediction as data table
  
  observeEvent(data$pred , {
    
    output$predictionData <- DT::renderDataTable({
      
      validate(need(class(data$pred) != "try-error" , "No prediction possible, please check if the features are similar to the training data!\n"))
      isolate({
        namesToExtract <- grep("prob.FALSE" , names(data$pred$data) , invert = T)
        
        d <- data$pred$data[,namesToExtract]
        
        
        colnames(d) <- gsub("prob.TRUE" , "probability" , colnames(d))
        colnames(d) <- gsub("response" , "prediction" , colnames(d))
        
        #grep removes the target collumn of the input data in order to avoid uncertainty
        d <- cbind(d , data$newdata[,grep(data$pred$task.desc$target , x = names(data$newdata) , invert = T)])
        
        data.prediction.download <<- d
        
        return(DT::datatable(data = d , filter = 'top',options = list(rownames= FALSE)))
      })
      
    } )
    
  })
  
  
  # Visualization of the selected rows in the corresponding data table
  
  observe({
    
    d <- data.prediction.download[input$predictionData_rows_selected , ]
    
    output$plotsPrediction <- renderUI({
      
      validate(need(!is.null(plotfun) , message = "No plot function found!\n"))
      
      if(is.null(d) || dim(d)[1] < 1 ){
        
        return(NULL)
        
      }else{
        
        lapply(1:nrow(d) ,  function (x, plotfun , data) {
          
          
          output[[paste0("plot1",x)]] <- renderPlot({
            
            temp <-  plotfun(data[x , ])
            
            
          } )
          
          plotOutput(paste0("plot1",x))
          
        } , plotfun = plotfun , data = d)
      }
    })
    
  })
  
  
  output$predict.Download <- downloadHandler(filename = function() {paste0(input$predict.csv$name , "predicted.csv")} , content = function(file){write.csv(data.prediction.download , file)})
  
  
  # Visualization of the prediction within a data table
  
  observeEvent(data$pred , {
    
    output$predictionDataNN <- DT::renderDataTable({
      
      validate(need(class(data$pred) != "try-error" , "No prediction possible, please check if the features are similar to the training data!\n"))
      isolate({
        
        namesToExtract <- grep("prob.FALSE" , names(data$pred$data) , invert = T)
        
        d <- data$pred$data[,namesToExtract]
        
        colnames(d) <- gsub("prob.TRUE" , "probability" , colnames(d))
        colnames(d) <- gsub("response" , "prediction" , colnames(d))
        
        #grep removes the target collumn of the input data in order to avoid uncertainty
        d <- cbind(d , data$newdata[,grep(data$pred$task.desc$target , x = names(data$newdata) , invert = T)])
        
        return(DT::datatable(data = d , filter = 'top', options = list(rownames= FALSE) ,selection = 'single'))
      })
      
    })
    
    
  })
  
  
  # Visualization of the nearest neighbors for the selected row in the corresponding data table
  
  observe({
    
    # it is necessary to revaluate the fgf_functions on the selected observations, since this data is not neseccarly available in the dataset
    
    if(!is.null(data$model$funList)){
      
      newData <- CurveClassification:::evaluateFunList(funList = data$model$funList , data = data.prediction.download[input$predictionDataNN_rows_selected , ])
      
    }else{
      
      newData <- data.prediction.download[input$predictionDataNN_rows_selected , ]
      
    }
    
    
    validate(need(!is.null(plotfun) , "No plot function avaiable!\n"),
             need(!is.null(data$model) , "No model selected!\n"),
             need(nrow(newData) > 0 , "No observation selected!"),
             need(!is.null(data$model$data) , "No data for the model available!"))
    colnames(newData) <- gsub("truth" , replacement = data$model$model$task.desc$target , x = colnames(newData)) 
    
    searchspace <- data$model$data[ , c(data$model$model$features , data$model$model$task.desc$target)]
    
    searchspace <- CurveClassification:::removeNAs(searchspace)
    
    neighbours <- try(nearestNeighbors(searchspace = searchspace , newData = newData , targetColumn = data$pred$task.desc$target , nNeighbor  = nrow(searchspace)))
    
    if(class(neighbours) != "try-error"){
      # get with the rownames the nearest neighbors from the original data which contain everything required for plotting
      neighbours <- data$model$data[rownames(neighbours) , ]
      #neighbours does not contain nessearyly the serach query therefore on position 3 the origdata is added
      #the grep is two times nesessary to ensure the same order of classes
      neighbours[3 , ] <- NA
      neighbours[3 , grep(paste0(names(newData) , collapse = "|") , names(neighbours))] <- newData[,grep(paste0(names(newData) , collapse = "|") , names(neighbours) , value = T)]
      
    }
    
    output$nearestNeighborTRUE <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      plotfun(neighbours[1,])
      
    })
    output$nearestNeighborEx <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      
      plotfun(neighbours[3,])
      
    })
    
    output$nearestNeighborFALSE <- renderPlot({
      
      if(class(neighbours) == "try-error"){
        return(NULL)
      }
      
      plotfun(neighbours[2,])
      
    })
    
  })
  
  
  
  # ##### data summary #####
  # 
  # # Mainly integrated form the mlr shiny package
  # 
  # # numeric variables
  # numericFeatures <- reactive({
  #   # req(data$data)
  #   d <- data$data
  #   return(colnames(Filter(is.numeric, d)))
  # })
  # 
  # # factor variables
  # factorFeatures <- reactive({
  #   # req(data$data)
  #   d <- data$data
  #   return(colnames(Filter(is.factor, d)))
  # })
  # 
  # output$data.summary.box <- renderUI({
  #   
  #   
  #   ui <- box(width = 12,
  #             
  #             htmlOutput("data.summary.caption"),
  #             DT::dataTableOutput("summary.datatable")
  #   )
  #   ui
  # })
  # 
  # data.summary <- reactive({
  #   req(data$data)
  #   d <- data$data
  #   validateData(d)
  #   colnames(d) <- make.names(colnames(d))
  #   pos.x <- colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
  #   d <- dropNamed(d, drop = pos.x)
  #   summarizeColumns(d)
  # })
  # 
  # output$data.summary.caption <- renderUI({
  #   capt <- sprintf("Your dataset contains %i observations. Click on one or more variables for visualisation!", nrow(data$data))
  #   helpText(capt)
  # })
  # 
  # output$summary.datatable <- DT::renderDataTable({
  #   data.summary()
  # }, options = list(scrollX = TRUE))# , caption = capt)
  # 
  # summary.vis.var <- reactive({
  #   req(data$data)
  #   d <- data$data
  #   pos.x <- colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
  #   d <- dropNamed(d, drop = pos.x)
  #   s <- summarizeColumns(d)
  #   s$name[input$summary.datatable_rows_selected]
  # })
  # 
  # output$summary.vis.hist = renderUI({
  #   list(
  #     column(3,
  #            radioButtons("summary.vis.dens", "Plot type", choices = c("Histogram", "Density"),
  #                         selected = "Histogram", inline = TRUE)
  #     ),
  #     column(9,
  #            sliderInput(inputId = "summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
  #                        value = 30L, step = 1L, width = "95%")
  #     )
  #   )
  # })
  # 
  # observeEvent(input$summary.vis.dens, {
  #   
  #   if (input$summary.vis.dens == "Density"){
  #     shinyjs::hide(id = "summary.vis.hist.nbins",animType = "fade")
  #   }else{
  #     shinyjs::show(id = "summary.vis.hist.nbins", animType = "fade")
  #   }
  # })
  # 
  # observeEvent(summary.vis.var(), {
  #   feature = summary.vis.var()
  #   if (length(feature) > 0L) {
  #     shinyjs::show("summary.vis.box", anim = TRUE)
  #     if (length(feature) == 1L) {
  #       if (feature %in% factorFeatures()) {
  #         shinyjs::hide("summary.vis.hist", animType = "fade")
  #       } else {
  #         shinyjs::show("summary.vis.hist", anim = TRUE)
  #       }
  #     } else
  #       shinyjs::hide("summary.vis.hist", animType = "fade")
  #   } else {
  #     shinyjs::hide("summary.vis.box", anim = TRUE)
  #   }
  # })
  # 
  # summary.vis.out = reactive({
  #   reqAndAssign(summary.vis.var(), "feature")
  #   d <- na.omit(data$data)
  #   reqNFeat(feature, d)
  #   barfill <- "#3c8dbc"
  #   barlines <- "#1d5a92"
  #   if (length(feature) == 1L) {
  #     if (feature %in% numericFeatures()) {
  #       reqAndAssign(input$summary.vis.dens, "density")
  #       x <- as.numeric(d[,feature])
  #       summary.plot <- ggplot(data = d, aes(x = x))
  #       
  #       if (density == "Density")
  #         summary.plot <- summary.plot + geom_density(fill = "blue", alpha = 0.1)
  #       else
  #         summary.plot <- summary.plot + geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins)
  #       
  #       summary.plot <- summary.plot + xlab(feature) +
  #         geom_vline(aes(xintercept = quantile(x, 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
  #         geom_vline(aes(xintercept = quantile(x, 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
  #         geom_vline(aes(xintercept = quantile(x, 0.5)), color = "blue", size = 1, linetype = "dashed")
  #       summary.plot <- addPlotTheme(summary.plot)
  #       summary.plot
  #     } else {
  #       class = d[,feature]
  #       summary.plot <- ggplot(data = d, aes(x = class)) +
  #         geom_bar(aes(fill = class), stat = "count") + xlab(feature) +
  #         guides(fill = FALSE)
  #       summary.plot <- addPlotTheme(summary.plot)
  #       summary.plot
  #     }
  #   } else if (length(feature) > 1L) {
  #     summary.plot <- ggpairs(data = d, columns = feature,
  #                             upper = list(continuous = wrap("cor", size = 10)),
  #                             lower = list(continuous = "smooth"))
  #     summary.plot
  #   }
  # })
  # 
  # output$summary.vis <- renderPlotly({
  #   ggplotly(summary.vis.out())
  # })
  # 
  # summary.vis.collection <- reactiveValues(var.plots = NULL)#var.names = NULL, var.plots = NULL)
  # 
  # observeEvent(summary.vis.out(), {
  #   q <- summary.vis.out()
  #   feat <- isolate(summary.vis.var())
  #   feat <- paste(feat, collapse = ".x.")
  #   
  #   # summary.vis.collection$var.names = c(summary.vis.collection$var.names, feat)
  #   summary.vis.collection$var.plots[[feat]] <- q
  #   
  #   
  # })
  # 
  # 
  
  ### Upload and processing of plotfunction ###
  
  observe({
    
    f <- input$plotScript$datapath
    
    # if a plotfunction is already sourced but a new is uploaded the enviroment is flushed and the new upload is sourced
    if(!is.null(f)){
      
      rm(list = ls(envir = plotfun_Env) ,envir =  plotfun_Env)
      
      source(f , local = plotfun_Env)
      
    }
    
    if(length(grep("^plot_" , ls(envir = plotfun_Env))) == 1){
      
      plotfun <<- plotfun_Env[[ (grep("^plot_" , ls(envir = plotfun_Env) , value = T))]]
    }
    
    output$plotEnv_list<-  DT::renderDataTable({
      
      validate(need(length(plotfun_Env)>0 , message = "No Elements in the Enviroment!\n"),
               need(length(grep("^plot_" , ls(envir = plotfun_Env))) == 1  , "One function with the name plot_ have to be provided!\n"))
      
      DT::datatable(t(ls(envir = plotfun_Env)) , options = list(paging = FALSE, searching = FALSE,
                                                                bInfo = FALSE, ordering = FALSE ))
    })
    
    output$testplot <- renderPlot({
      
      validate(need(!is.null(data$data) , message = "No data avaiable!\n"),
               need(!is.null(plotfun) , message = "No plot function avaiable!\n"))
      
      plotfun(data$data[1,])
      
    })
    
    output$plotfuncode <- renderPrint({
      
      validate(need(!is.null(plotfun), message = "No plot function avaiable!\n"))
      
      for(n in ls(envir = plotfun_Env)){
        
        print(plotfun_Env[[n]])
        
      }
      
    })
    
  })
  
  
  
  ### visualization of model parameters ###
  
  output$Hyperpars <- DT::renderDataTable({
    
    validate(need(!is.null(data$model) , "No model loaded"))
    d <- summarizeModel(data$model) 
    rownames(d)<- NULL
    DT::datatable(d , options = list(paging = FALSE, searching = FALSE,
                                     bInfo = FALSE, ordering = FALSE , rownames = FALSE ))
  } )
  
  output$Features <- DT::renderDataTable({
    
    validate(need(!is.null(data$model) , "No model loaded"))
    d <- t(as.data.frame(data$model$model$features))
    rownames(d)<- NULL
    DT::datatable(d , options = list(paging = FALSE, searching = FALSE,
                                     bInfo = FALSE, ordering = FALSE , colnames = FALSE ))
  } , rownames= FALSE)
  
  
  output$fgf_calls <- renderPrint({
    validate(need(!is.null(data$model) , "No model loaded"))
    
    data$model$funList
    
    
  })
  
  
})
