

require(shinyjs)

shinyUI( 
  
  fluidPage(style = pagewidth,
            tags$head(tags$style("#container * {display: inline ;}" ),
                      tags$style(type="text/css", "
                                       #loadmessage {
                                       position: fixed;
                                       top: 50px;
                                       left: 0px;
                                       width: 100%;
                                       padding: 5px 0px 5px 0px;
                                       text-align: center;
                                       font-weight: bold;
                                       font-size: 100%;
                                       color: #000000;
                                       background-color: #CCFF66;
                                       z-index: 105;
                                       }
                                       "),
                      tags$style(HTML("
                                      .shiny-output-error-validation {
                                      color: red;font-weight: bold;
                                      }
                                      ")),
                      tags$link(rel = "icon" , href="curve.png"),
                      tags$title("Curve classification tool")
            ),
            useShinyjs(),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             
                             tags$div("Loading...",id="loadmessage")),
            navbarPage('Curve classification' ,   
                      
                       tabPanel('Data import' ,value = 1 , 
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 uiOutput("import.ui")
                                    ),
                                    mainPanel(width = 9,
                                              
                                              box(width = 12, DT::dataTableOutput("import.preview"))
                                    )
                                    
                                  )
                                )
                                
                       ),
                       tabPanel('Feature generation'  ,
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 uiOutput("fgf.ui")
                                    ),
                                    mainPanel(width = 9,
                                              wellPanel(
                                                h4("Messages") ,
                                                span(textOutput("fgf.messages") , style="color:red")),
                                              box(width = 12, DT::dataTableOutput("fgf.preview")
                                              ),br(),

                                              plotOutput("fgf.beanplot" , height = 700 )


                                    )

                                  )
                                )

                       ) ,
                       tabPanel('Generate new model' ,
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 uiOutput("newModel.ui")
                                    ),
                                    mainPanel(width = 9 ,
                                              wellPanel(
                                                h4("Messages") ,
                                                span(htmlOutput("newModelMessages" ) )
                                              ),
                                              br(),
                                              tabsetPanel(tabPanel("Performance" ,
                                                                   h4("Confusion matrix"),

                                                                   verbatimTextOutput("ConfusionMatrix"), br(),
                                                                   
                                                                   h4("Performance measures"),
                                                                   
                                                                  tableOutput("PerformanceMeasures"),
                                                                  br(), 
                                                                   
                                                                    h4("Performance vs. Threshold"),

                                                                   plotOutput("performanceVsThreshold", width = "60%" , height = "600px"),
                                                                       br(),

                                                                       h4("Probability distribution"),
                                                                       plotOutput("probabilityDistribution" , width = "60%", height = "600px")
                                                                   ),
                                                          tabPanel("Data" ,

                                                                   DT::dataTableOutput("generateModelData"),
                                                                   uiOutput("plotsGenerateModels")
                                                                
                                                                   

                                                                   ),
                                                          tabPanel("Nearest Neighbors",
                                                                   
                                                                   DT::dataTableOutput("NNgenerateModelData"),
                                                                   h3("Nearest neighbor plots of the selected observation"),
                                                                   br(),
                                                                   splitLayout(h4("nearest neighbor: TRUE") , h4("Selected observation") , h4("nearest neighbor: FALSE"), cellWidths = c("33%", "33%" ,"33%" )),
                                                                   splitLayout(plotOutput("nearestNeighborTRUEGenNewMod")  , plotOutput("nearestNeighborExGenNewMod")  ,plotOutput("nearestNeighborFALSEGenNewMod") , cellWidths = c("33%", "33%" ,"33%" ))
                                                          )




                                                          )



                                    )

                                  )
                                )
                       )  ,
                       tabPanel('Optimize existing model'  ,


                                sidebarPanel(width = 3,
                                             uiOutput("optimize.ui"),
                                             uiOutput("optimizeExchangeButton")
                                ),
                                mainPanel(width = 9    ,
                                          wellPanel(
                                            h4("Messages"),
                                            htmlOutput("optimizeMessages" )  ),
                                          tabsetPanel(


                                            tabPanel("Performance" ,
                                                     column(width = 4.5 ,
                                                            box(

                                                              h4("Old model"),

                                                              h5("Confusion matrix"),

                                                              verbatimTextOutput("ConfusionMatrix.old"), br(),
                                                              h5("Performance measures"),
                                                              
                                                              tableOutput("PerformanceMeasures.old"),
                                                              br(), 
                                                              
                                                              h5("Performance vs. Threshold"),

                                                             plotOutput("performanceVsThreshold.old"),
                                                                  br(),

                                                                  h5("Probability distribution"),
                                                                  plotOutput("probabilityDistribution.old")


                                                            )


                                                     ),
                                                     column(width = 4.5 ,
                                                            box(

                                                              h4("New model"),

                                                              h5("Confusion matrix"),

                                                              verbatimTextOutput("ConfusionMatrix.new"), br(),
                                                              
                                                              h5("Performance measures"),
                                                              
                                                              tableOutput("PerformanceMeasures.new"),
                                                              br(), 
                                                              
                                                              h5("Performance vs. Threshold"),


                                                              plotOutput("performanceVsThreshold.new"),
                                                                  br(),

                                                                  h5("Probability distribution"),
                                                                  plotOutput("probabilityDistribution.new")

                                                            )


                                                     )
                                            ),
                                            tabPanel("Data" ,

                                                     DT::dataTableOutput("optimizeNewdata"),
                                                     uiOutput("plotsOptimizeModel")

                                            ),
                                            tabPanel("Nearest Neighbors",
                                                     
                                                     DT::dataTableOutput("NNoptimizeNewdata"),
                                                     h3("Nearest neighbor plots of the selected observation"),
                                                     br(),
                                                     splitLayout(h4("nearest neighbor: TRUE") , h4("Selected observation") , h4("nearest neighbor: FALSE"), cellWidths = c("33%", "33%" ,"33%" )),
                                                     splitLayout(plotOutput("nearestNeighborTRUEOptimizeMod")  , plotOutput("nearestNeighborExOptimizeMod")  ,plotOutput("nearestNeighborFALSEOptimizeMod") , cellWidths = c("33%", "33%" ,"33%" ))
                                            )
                                          )


                                )



                       )   ,
                       tabPanel("Validate Model",

                                sidebarPanel(width = 3 ,

                                             uiOutput("validate.ui")

                                             ),
                                mainPanel(width = 9,
                                          tabsetPanel(
                                            tabPanel("False Positives" ,
                                                     uiOutput("plotsFP")),
                                            tabPanel("False Negatives" ,
                                                     uiOutput("plotsFN"))
                                          ))



                                ),
                       tabPanel("Predict" ,

                                sidebarPanel(width = 3,
                                             uiOutput("predict.ui")
                                ),
                                mainPanel(width = 9 ,
                                          tabsetPanel(
                                            tabPanel("prediction" ,

                                                     DT::dataTableOutput("predictionData"),
                                                     uiOutput("plotsPrediction")


                                            ) ,
                                            tabPanel("Nearest Neighbors",

                                                     DT::dataTableOutput("predictionDataNN"),
                                                     h3("Nearest neighbor plots of the selected observation"),
                                                     br(),
                                                     splitLayout(h4("nearest neighbor: TRUE") , h4("Selected observation") , h4("nearest neighbor: FALSE"), cellWidths = c("33%", "33%" ,"33%" )),
                                                     splitLayout(plotOutput("nearestNeighborTRUE")  , plotOutput("nearestNeighborEx")  ,plotOutput("nearestNeighborFALSE") , cellWidths = c("33%", "33%" ,"33%" ))


                                            ))


                                )
                       ),
                       navbarMenu("More" ,
                                  tabPanel("Explore data" ,
                                           bootstrapPage(

                                             uiOutput("data.summary.box"),
                                             box(width = 12, title = "Variable Visualization", id = "summary.vis.box",
                                                 fluidRow(
                                                   column(12,
                                                          uiOutput("summary.vis.hist")),
                                                   column(12,
                                                          plotlyOutput("summary.vis")
                                                   )
                                                 )
                                             )

                                           )




                                  ),
                                  tabPanel("Others" ,
                                           tabsetPanel(
                                             tabPanel("Upload plot function"  ,

                                                      sidebarPanel( width = 3 ,
                                                                    fileInput("plotScript" , label = "Select a plot script")
                                                      ),
                                                      mainPanel(width = 9 ,
                                                                h4("Objects sourced from the script"),
                                                                DT::dataTableOutput("plotEnv_list") ,br() ,  h4("Testplot") ,
                                                                plotOutput("testplot"),
                                                                h4("Plot function"),
                                                                verbatimTextOutput("plotfuncode"))
                                             ),
                                             tabPanel("Model parameters" ,
                                                      h3("Hyperparameters"),
                                                      DT::dataTableOutput("Hyperpars"),
                                                      br(),
                                                      h3("Used features"),
                                                      DT::dataTableOutput("Features"),
                                                      br(),
                                                      h3("Feature generation function calls"),
                                                      verbatimTextOutput("fgf_calls")

                                                      )
                                           ))
                       )

                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
            )
  )
)
