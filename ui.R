

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
                      tags$link(rel = "icon" , href="curve.png"),
                      tags$title("Curve classification tool")
            ),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage")),
            navbarPage('Curve classification' ,   
                      
                       tabPanel('Data import' ,value = 1  , icon = icon(name = "fa-database" ,class = "fa fa-database" , lib = "font-awesome") , 
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
                       tabPanel('Feature generation' ,icon = icon(name = "fa-area-chart" ,class = "fa fa-area-chart" , lib = "font-awesome") ,
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
                       tabPanel('Generate new model'  , icon = icon(name = "fa-calculator" ,class = "fa fa-calculator" , lib = "font-awesome"),
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 uiOutput("newModel.ui")
                                    ),
                                    mainPanel(width = 9 , 
                                              
                                              wellPanel(
                                                h4("Messages") , 
                                                span(textOutput("newModelMessages" ) , style="color:red")
                                              ),
                                              br(),
                                              
                                              
                                              h4("Confusion matrix"),
                                              
                                              tableOutput("ConfusionMatrix"), br(), 
                                              
                                              
                                              box(plotOutput("performanceVsThreshold"),
                                                  br(),
                                                  
                                                  h4("Probability distribution"), 
                                                  plotOutput("probabilityDistribution"))
                                              
                                              
                                              
                                    )
                                    
                                  )
                                )
                       )  , 
                       tabPanel('Optimize existing model'  , icon = icon(name = "fa-calculator" ,class = "fa fa-calculator" , lib = "font-awesome"),
                                
                                
                                sidebarPanel(width = 3,
                                             uiOutput("optimize.ui"),
                                             uiOutput("optimizeExchangeButton")
                                ),
                                mainPanel(width = 9    , 
                                          wellPanel(
                                            h4("Messages"),
                                            span(textOutput("optimizeMessages" ) , style="color:red") ),
                                          tabsetPanel(
                                            tabPanel("reevaluated data" , 
                                                     
                                                     DT::dataTableOutput("optimizeNewdata")
                                                     
                                            ),
                                            
                                            tabPanel("model performance" , 
                                                     column(width = 4.5 , 
                                                            box(
                                                              
                                                              h4("Old model"),
                                                              
                                                              h5("Confusion matrix"),
                                                              
                                                              tableOutput("ConfusionMatrix.old"), br(), 
                                                              
                                                              
                                                              box(plotOutput("performanceVsThreshold.old"),
                                                                  br(),
                                                                  
                                                                  h5("Probability distribution"), 
                                                                  plotOutput("probabilityDistribution.old"))
                                                              
                                                              
                                                              
                                                              
                                                            )
                                                            
                                                            
                                                     ),
                                                     column(width = 4.5 , 
                                                            box(
                                                              
                                                              h4("New model"),
                                                              
                                                              h5("Confusion matrix"),
                                                              
                                                              tableOutput("ConfusionMatrix.new"), br(), 
                                                              
                                                              
                                                              box(plotOutput("performanceVsThreshold.new"),
                                                                  br(),
                                                                  
                                                                  h5("Probability distribution"), 
                                                                  plotOutput("probabilityDistribution.new"))
                                                              
                                                              
                                                              
                                                              
                                                            )
                                                            
                                                            
                                                     )
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
                                                      mainPanel(width = 9 , tableOutput("plotEnv_list"))
                                             )
                                           ))
                       )
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
            )
  )
)
