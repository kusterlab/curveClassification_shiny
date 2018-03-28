library(shiny)
require(shinydashboard)
require(plotly)
require(BBmisc)
require(mlr)
require(GGally)
require(shinyjs)

#source("V://users_files/Florian Seefried/Master/R_project/Masterthesis/functions/GenerateModel.R")

#source("V://users_files/Florian Seefried/Master/R_project/Masterthesis/functions/GlobalFeatureGenerationFunctions.R")

source("V://users_files/Florian Seefried/Master/R_project/Masterthesis/functions/ConvertFeatureClass.R")

source("V://users_files/Florian Seefried/Master/R_project/Masterthesis/functions/getThreshold.R")

source("./configuration.R")

shinyServer(function(input, output , session) {
  
  
  # 'size and 'datapath'
  
  ##### data import #####
  
  data <- reactiveValues(data = NULL , newFeatures = NULL , model = NULL , train = NULL , test = NULL , newdata = NULL , pred = NULL , evaluated = NULL , modelretrained = NULL)
  
  fgf.List <- NULL
  
  data.prediction.download <- NULL
  
  output$import.ui <- renderUI({
    
    sidebarMenu(
      
      fileInput("import.csv", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      checkboxInput("import.header", "Header", TRUE),
      
      selectInput("import.sep", "Separator", selected = ",",
                  
                  choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
      
      selectInput("import.quote", "Quote", selected = '"',
                  
                  choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'")))
    
  })
  
  
  observe({ 
    
    f = input$import.csv$datapath
    
    if (is.null(f)) {
      
      data$data <- NULL
      
    } else {
      
      data$data <- read.csv(f, header = input$import.header, sep = input$import.sep,
                            quote = input$import.quote)
    }
  })
  
  
  
  output$import.preview <- DT::renderDataTable({
    if(!is.null(data$data)){
      #To ensure that the data is already not NULL
      reqAndAssign(data$data , "d")
      #providing columnNames
      colnames(d) <- make.names(colnames(d))
      #nessesary to return the dataframe
      d
    }
  }, options = list(scrollX = TRUE),
  caption = "You imported the following data set")
  
  ##### feature generation #####
  
  output$fgf.ui <- renderUI({
    
    #Nessesary to reavalute all after the R script is sourced
    input$fgf.R$datapath
    #
    sidebarMenu(
      
      fileInput("fgf.R", "Choose R script",
                accept = c(".R")),
      
      
      selectInput("fgf.function", "Feature generation function", selected = NULL,
                  
                  choices = grep("^fgf_" , ls(envir = globalenv() ) , value = T) , multiple = T , selectize = T),
      selectInput("fgf.counts", "Feature generation function counts", selected = NULL,
                  
                  choices = grep("^fgf_" , ls(envir = globalenv() ) , value = T) , multiple = T , selectize = T),
      textInput("fgf.concentrations.Pattern" , "Feature generation concentration patterns" , placeholder = "e.g. Normalized.LFQ.intensity.;Normalized.Intensity."),
      
      textInput("fgf.counts.Pattern" , "Feature generation count patterns" , placeholder = "e.g. MS.MS.count.;Unique.peptides."),
      
      actionButton("fgf.start" , label = "Generate features"),
      br(),
      br(),
      "Please make sure to do not change anything in feature generation after you clicked the button 'Use data with new features' or redo the whole process if you like to change anyting.",
      br(),
      br(),
      actionButton("fgf.replace" , label = "Use data with new features")
    )
    
  })
  
  observe({ 
    
    f = input$fgf.R$datapath
    
    if (is.null(f)) {
      
      NULL
      
    } else {
      
      
      try(source(f,  local = F))
      
    }
  })
  
  
  observeEvent(input$fgf.start , {
    
    output$fgf.messages <- renderText(isolate({
      
      patternCounts <- strsplit(input$fgf.counts.Pattern , ";")[[1]] 
      
      patternConcen <- strsplit(input$fgf.concentrations.Pattern , ";")[[1]]
      
      fgf.List <<- NULL
      
      data$newFeatures <<- NULL
      
      if(!is.null(input$fgf.concentrations)){
        
        validate(need({length(grep(paste0(patternConcen , collapse = "|") , names(data$data))) > 0}, message = "The specified pattern for concentration dependet features does not occur in the dataset"))
        
        
        fgf.List <<- generateFunctionList(input$fgf.concentrations , patternConcen)
        
      }
      
      if(!is.null(input$fgf.counts)){
        
        validate(need({length(grep(paste0(patternCounts , collapse = "|") , names(data$data))) > 0}, message = "The specified pattern for count dependet features does not occur in the dataset"))
        
        
        fgf.List <<- c(generateFunctionList(input$fgf.concentrations , patternCounts))
        
      }
      
      validate(need(!is.null(fgf.List), message = "No feature generation functions selected"),
               need(!is.null(data$data) , message = "No data set selected"))
      
      
      d <- try(evaluateFunList(fgf.List , data$data) , silent = T)
      
      validate(need(class(d) != "try-error" , message = "An error occured during feature calculation no features calculated!"))
      data$newFeatures <<- d
      
      return(NULL)
      
      
    }))
  })
  
  observeEvent(input$fgf.replace , {
    
    req(!is.null(data$newFeatures))
    data$data <<- data$newFeatures
    
    
    
  })
  
  
  output$fgf.preview <- DT::renderDataTable({
    
    
    if(!is.null(data$newFeatures)){
      #To ensure that the data is already not NULL
      reqAndAssign(data$newFeatures , "d")
      #providing columnNames
      colnames(d) <- make.names(colnames(d))
      #nessesary to return the dataframe
      d
    }
  }, options = list(scrollX = TRUE),
  caption = "You generated the following new dataset")
  
  
  
  
  
  ##### generate new models #####
  
  features <- reactive({
    if(is.null(input$newModel.features)){
      return(NULL)
    }else{
      return(input$newModel.features)
    }
    
  })
  
  TargetColumn <- reactive({
    if(is.null(input$newModel.TargetColumn)){
      return(NULL)
    }else{
      return(input$newModel.TargetColumn)
    }
    
  })
  
  
  PositiveClass <- reactive({
    if(is.null(input$newModel.PositiveClass)){
      return(NULL)
    }else{
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
    
    if(is.null(data$data)){
      
      return(NULL)
      
    }else if(is.null(input$newModel.TargetColumn) || is.null(input$newModel.PositiveClass)){
      
      return(0.05)
      
    }else{
      
      return(sum(data$data[,input$newModel.TargetColumn] == input$newModel.PositiveClass)/dim(data$data)[1])
    
      }
    
  })
  
  
  output$newModel.ui <- renderUI({
    
    sidebarMenu(
      
      selectInput("newModel.features" , label = "Select features" , choices = colnames(data$data) , multiple = T , selected = isolate(features()) , selectize = T) , 
      
      selectInput("newModel.TargetColumn" , label = "Select target" , choices = colnames(data$data) , multiple = F , selected = TargetColumn() , selectize = T),
      
      selectInput("newModel.PositiveClass" , label = "Select positive class" , choices = unique(data$data[,input$newModel.TargetColumn]) , multiple = F , selected = PositiveClass() , selectize = T),
      
      numericInput("newModel.splitData" , label = "Ratio to split data into train and test" , min = 0 , max = 1 , step = 0.05 , value = 0.8),
      
      numericInput("newModel.usRate" , label = "Select a undersampling rate" , value = usRate() , min = 10^-4 , max = 1 , step = 10^-4) , 
      
      checkboxInput("newModel.tuneThreshold" , label = "Tune threshold?" , value = tuneThreshold()),
      
      if(!is.null(input$newModel.tuneThreshold) && input$newModel.tuneThreshold){
        
        numericInput("newModel.tprTuneValue" , label = "Tpr tune value" , min = 0 , max = 1 ,value = 0.995 ,  step = 10^-3)
      },
      
      
      actionButton("newModel.train" , label = "train"),
      
      if(!is.null(data$model)){
        
        downloadButton("saveModel" , "Save model")
        
      }
      
      
      
      
    )
    
    
  })
  
  output$saveModel <- downloadHandler(filename = function(){"model.RData"} , content = function(file){
    
    saveRDS(data$model , file = file)
    
  })
  
  observeEvent(input$newModel.train , {
    
    output$newModelMessages <- renderText({
     
       txt <- NULL
      
        if(is.null(features()) || any(features() %in% TargetColumn()) ){
          
          txt <- "You need to select features and the target variable could not be a feature.\n"
        }
        
        if(is.null(usRate()) || usRate() == 0){
          
          txt <- c(txt , "The selected positive class does not contain a single observations.\n")
          
        }
       
       if(is.null(data$data)){
         
         
         txt <- c(txt , "Please select a dataset.\n")
         
         
       }
      
      if(length(txt)>0){
        
        return(txt)
      }
        
        if(!is.null(features()) && !any(features() %in% TargetColumn()) && usRate() != 0){
          
          isolate({
          n <- sample(1:dim(data$data)[1] , 0.8*dim(data$data)[1])
          
          d <- data$data
          
          d[,TargetColumn()] <- ifelse(d[,TargetColumn()] == PositiveClass() , TRUE , FALSE)
          
          
          data$train <- d[ n , ]
          
          data$test <- d[ setdiff( 1:dim(data$data)[1] , n) , ]
          
          data$model <- generateModel(classifier = "classif.randomForest" , us.rate = usRate() , features = features() , data = data$train , targetVariable = TargetColumn() ,positiveClass = "TRUE" , estimatingThreshold = tuneThreshold() , tprThreshold = input$newModel.tprTuneValue )
          
          
          data$model <- combineModel(trainOutput = data$model , featureFunctionList = fgf.List , test.data = data$test , positveClass = PositiveClass())
          
          
          return("Model trained successfull!")
          })
          
          
        }
        
      
      
    })
    
    
    
  })
  
  observeEvent( data$model, {
    
    pred <- predict(data$model , newdata = data$model$test.data)
    
    if(!is.null(data$model$threshold)){
      
      pred <- setThreshold(pred , threshold = data$model$threshold)
      
      
    }
    
    
    output$ConfusionMatrix <- renderTable({
      calculateConfusionMatrix(pred)$result
    } , rownames = T , digits = 0)
    
    output$performanceVsThreshold <- renderPlot({
      
      return(plotThreshVsPerf(generateThreshVsPerfData(pred , measures = list(tpr , fpr , ppv , acc)) , pretty.names = T))
      
      
      })
    
    
    output$probabilityDistribution <- renderPlot({
     
        
        return( plotTargetDensity(pred , thresholdLFQ = pred$threshold["TRUE"]) )
        
        
     
    })
    
    
  })
  #### optimize model ####
  
  output$optimize.ui <- renderUI({
    
    sidebarMenu(
      
      fileInput("optimize.model" , "Choose model" , accept = ".RData"),
      
      fileInput("optimize.newdata" , "Choose reannotated data" , accept = ".csv"),
      
      checkboxInput("optimize.header", "Header", TRUE),
      
      selectInput("optimize.sep", "Separator", selected = ",", choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
      
      
      checkboxInput("optimize.tuneThreshold" , label = "Tune threshold?" , value = tuneThresholdOptimize()),
      
      if(!is.null(input$optimize.tuneThreshold) && input$optimize.tuneThreshold){
        
        numericInput("optimize.tprTuneValue" , label = "Tpr tune value" , min = 0 , max = 1 ,value = 0.995 ,  step = 10^-3)
      },
      
      actionButton("optimize.retrain" , label = "optimize") 
      
    

      
    )

    
    
  })
  
  
  observe({
    
    f = input$optimize.newdata$datapath
    
    if (is.null(f)) {
      
      data$evaluated <- NULL
      
    } else {
      
      data$evaluated <- read.csv(f, header = input$optimize.header, sep = input$optimize.sep)
    }
    
    
    
  })
  
  observe({
    
    f = input$optimize.model$datapath
    
    if (!is.null(f)) {
      
      data$model <- readRDS(f)
      
    }
    
    
    
  })
  

  tuneThresholdOptimize <- reactive({
    
    if(is.null(input$optimize.tuneThreshold)){
      return(FALSE)
    }else{
      return(input$optimize.tuneThreshold)
    }
    
    
  })
  
  
  observeEvent(input$optimize.retrain , {
    
    
    output$optimizeMessages <- renderText({
      
      
      
      if(is.null(data$evaluated) || is.null(data$model) ){
        
        txt <- "You need to select a model and/or new annotated data.\n"
        return(txt)
      }
      
      
      
      
        
        isolate({
          
          d <- data$evaluated
          
          
          d[,data$model$model$task.desc$target] <- ifelse(d[,data$model$model$task.desc$target] == data$model$positiveClass , TRUE , FALSE)
          
          data$modelretrained <- retrain(combinedModel = data$model , newdata = d , estimatingThreshold = tuneThresholdOptimize() , tprThreshold = input$optimize.tprTuneValue)
          

          

          
          return("Model retrained successfull")
        })

      
    })
    

    
  })
  
  observeEvent(data$modelretrained , {
    
    pred <- predict(data$model , newdata = data$modelretrained$test.data)
    
    predNewMod <- predict(data$modelretrained , newdata = data$modelretrained$test.data)
    
    if(!is.null(data$model$threshold)){
      
      pred <- setThreshold(pred , threshold = data$model$threshold)
      
      
    }
    if(!is.null(data$modelretrained$threshold)){
      
      predNewMod <- setThreshold(predNewMod , threshold = data$modelretrained$threshold)
      
      
    }
    
    
    output$ConfusionMatrix.old <- renderTable({
      calculateConfusionMatrix(pred)$result
    } , rownames = T , digits = 0)
    
    output$performanceVsThreshold.old <- renderPlot({
      
      return(plotThreshVsPerf(generateThreshVsPerfData(pred , measures = list(tpr , fpr , ppv , acc)) , pretty.names = T))
      
      
    })
    
    
    output$probabilityDistribution.old <- renderPlot({
      
      
      return( plotTargetDensity(pred , thresholdLFQ = pred$threshold["TRUE"]) )
      
      
      
    })
    
    
    output$ConfusionMatrix.new <- renderTable({
      calculateConfusionMatrix(predNewMod)$result
    } , rownames = T , digits = 0)
    
    output$performanceVsThreshold.new <- renderPlot({
      
      return(plotThreshVsPerf(generateThreshVsPerfData(predNewMod , measures = list(tpr , fpr , ppv , acc)) , pretty.names = T))
      
      
    })
    
    
    output$probabilityDistribution.new <- renderPlot({
      
      
        return( plotTargetDensity(predNewMod , thresholdLFQ = predNewMod$threshold["TRUE"]) )
      
      
      
    })
    
    
    
  })
  
  
  
   output$optimizeNewdata <-  DT::renderDataTable(data$evaluated, caption = "New annotated data")
      
    
    
  output$optimizeExchangeButton <- renderUI({
    
    if(!is.null(data$modelretrained)){
      sidebarMenu(
      actionButton("optimizeExchangeButton" , label = "Use new model"),
      
      downloadButton("optimizeSaveModel" , label = "Download model")
      )
    }else{
      NULL
    }
    
    
  })
  
  observeEvent(input$optimizeExchangeButton , {
    
    data$model <- data$modelretrained
    
    data$modelretrained <- NULL
    
    data$evaluated <- NULL
    
    
    
    
  })
  
  output$optimizeSaveModel<- downloadHandler(filename = function(){"reevaluatedModel.RData"} , content = function(file){
    saveRDS(data$modelretrained , file = file)
    
  })
    
    
    
    
    
 
  
  
  #### predict #####
  
  output$predict.ui <- renderUI({
    
    sidebarMenu(
      
      fileInput("predict.csv", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv") , multiple = T),
      
      fileInput("predict.model" , "Choose model" , accept = ".RData"),
      
      checkboxInput("predict.header", "Header", TRUE),
      
      selectInput("predict.sep", "Separator", selected = ",", choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
      
      if(!is.null(data$newdata) && !is.null(data$model) ){
        list(
        actionButton("predict.go" , "predict"),
        downloadButton("predict.Download" , label = "Download .csv")
        )
      }
      
      )
    
  })
  
  
  observe({ 
    
    f = input$predict.csv$datapath
    
    if (is.null(f)) {
      
      data$newdata <- NULL
      
    } else {
      
      data$newdata <- read.csv(f, header = input$predict.header, sep = input$predict.sep)
    }
    
  })
  
  observe({
    
    f = input$predict.model$datapath
    
    if (!is.null(f)) {
      
      data$model <- readRDS(f)
      
      
    }
    
    
  })
  
  
  observeEvent(input$predict.go ,{
    
    
    data$pred <- try(predict(data$model , newdata = data$newdata))
 
    
  })
  
  observeEvent(data$pred , {
    
    output$predictionData <- DT::renderDataTable({
    
    validate(need(class(data$pred) != "try-error" , "No prediction possible, please check if the features are similar to the training data"))
      isolate({
      
      d <- data$pred$data[,c("prob.TRUE" , "response")]
      
      
      colnames(d) <- c("probability" , data$pred$task.desc$target )
      
      d <- cbind(data$newdata , d)
      
      d[,ncol(d)] <- ifelse(d[,ncol(d)] == "TRUE" , data$model$positiveClass , "")
      
      data.prediction.download <<- d
      
      return(d)
      })
      
    })
    
    
  })
  
  
  output$predict.Download <- downloadHandler(filename = function() {paste0(input$predict.csv$name , "predicted.csv")} , content = function(file){write.csv(data.prediction.download , file)})
  
  ##### data summary #####
  
  # numeric variables
  numericFeatures <- reactive({
    # req(data$data)
    d <- data$data
    return(colnames(Filter(is.numeric, d)))
  })
  
  # factor variables
  factorFeatures <- reactive({
    # req(data$data)
    d <- data$data
    return(colnames(Filter(is.factor, d)))
  })
  
  output$data.summary.box <- renderUI({
    
    
    ui <- box(width = 12, 
              
              htmlOutput("data.summary.caption"),
              DT::dataTableOutput("summary.datatable")
    )
    ui
  })
  
  data.summary <- reactive({
    reqAndAssign(data$data , "d")
    validateData(d)
    colnames(d) <- make.names(colnames(d))
    pos.x <- colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
    d <- dropNamed(d, drop = pos.x)    
    summarizeColumns(d)
  })
  
  output$data.summary.caption <- renderUI({
    capt <- sprintf("Your dataset contains %i observations. Click on one or more variables for visualisation!", nrow(data$data))
    helpText(capt)
  })
  
  output$summary.datatable <- DT::renderDataTable({
    data.summary()
  }, options = list(scrollX = TRUE))# , caption = capt)
  
  summary.vis.var <- reactive({
    reqAndAssign(data$data, "d")
    pos.x <- colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
    d <- dropNamed(d, drop = pos.x)
    s <- summarizeColumns(d)
    s$name[input$summary.datatable_rows_selected]
  })
  
  output$summary.vis.hist = renderUI({
    list(
      column(3,
             radioButtons("summary.vis.dens", "Plot type", choices = c("Histogram", "Density"),
                          selected = "Histogram", inline = TRUE)
      ),
      column(9,
             sliderInput(inputId = "summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
                         value = 30L, step = 1L, width = "95%")
      )
    )
  })
  
  observeEvent(input$summary.vis.dens, {
    #Does not work for some reasons not sure why
    
    if (input$summary.vis.dens == "Density"){
      shinyjs::hide(id = "summary.vis.hist.nbins",animType = "fade")
    }else{
      shinyjs::show(id = "summary.vis.hist.nbins", animType = "fade")
    }
  })
  
  observeEvent(summary.vis.var(), {
    feature = summary.vis.var()
    if (length(feature) > 0L) {
      shinyjs::show("summary.vis.box", anim = TRUE)
      if (length(feature) == 1L) {
        if (feature %in% factorFeatures()) {
          shinyjs::hide("summary.vis.hist", animType = "fade")
        } else {
          shinyjs::show("summary.vis.hist", anim = TRUE)
        }
      } else
        shinyjs::hide("summary.vis.hist", animType = "fade")
    } else {
      shinyjs::hide("summary.vis.box", anim = TRUE)
    }
  })
  
  summary.vis.out = reactive({
    reqAndAssign(summary.vis.var(), "feature")
    d <- na.omit(data$data)
    reqNFeat(feature, d)
    barfill <- "#3c8dbc"
    barlines <- "#1d5a92"
    if (length(feature) == 1L) {
      if (feature %in% numericFeatures()) {
        reqAndAssign(input$summary.vis.dens, "density")
        x <- as.numeric(d[,feature])
        summary.plot <- ggplot(data = d, aes(x = x))
        
        if (density == "Density")
          summary.plot <- summary.plot + geom_density(fill = "blue", alpha = 0.1)
        else
          summary.plot <- summary.plot + geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins)
        
        summary.plot <- summary.plot + xlab(feature) +
          geom_vline(aes(xintercept = quantile(x, 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
          geom_vline(aes(xintercept = quantile(x, 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
          geom_vline(aes(xintercept = quantile(x, 0.5)), color = "blue", size = 1, linetype = "dashed")
        summary.plot <- addPlotTheme(summary.plot)
        summary.plot
      } else {
        class = d[,feature]
        summary.plot <- ggplot(data = d, aes(x = class)) + 
          geom_bar(aes(fill = class), stat = "count") + xlab(feature) +
          guides(fill = FALSE)
        summary.plot <- addPlotTheme(summary.plot)
        summary.plot
      }
    } else if (length(feature) > 1L) {
      summary.plot <- ggpairs(data = d, columns = feature,
                              upper = list(continuous = wrap("cor", size = 10)), 
                              lower = list(continuous = "smooth"))
      summary.plot
    }
  })
  
  output$summary.vis <- renderPlotly({
    ggplotly(summary.vis.out())
  })
  
  summary.vis.collection <- reactiveValues(var.plots = NULL)#var.names = NULL, var.plots = NULL)
  
  observeEvent(summary.vis.out(), {
    q <- summary.vis.out()
    feat <- isolate(summary.vis.var())
    feat <- paste(feat, collapse = ".x.")
    
    # summary.vis.collection$var.names = c(summary.vis.collection$var.names, feat)
    summary.vis.collection$var.plots[[feat]] <- q
    
    
  })
  
  
  
  
  
  
})
