library(shiny)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Modeling"),
  windowTitle = "dHRUM",
  sidebarLayout(position = "left",
                sidebarPanel(width = 4,
                             h4("Choose a basin:"),
                             fluidRow(
                               column(width = 12, selectInput("basin", label = NULL, choices = c("BP basin","KL basin")))
                             ),
                             h4("Adjust parameters values:"),
                             fluidRow(
                               tabsetPanel(
                                 tabPanel(
                                   "Direct Runoff",
                                   fluid = F,
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 12,
                                       sliderInput("ks", "KS:", min = 0, max = 1, value = 0.001, step = 0.001),
                                       sliderInput("kf", "KF:", min = 0, max = 1, value = 0.3, step = 0.01),
                                       sliderInput("adiv", "ADIV:", min = 0, max = 1, value = 0.4, step = 0.01)
                                     ),
                                     mainPanel(width = 0)
                                   )
                                 ),
                                 tabPanel(
                                   "Vegetation" ,
                                   fluid = F,
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 12,
                                       sliderInput("cans_st", "CANS_ST:", min = 0, max = 5, value = 1.5, step = 0.01),
                                       sliderInput("stem_st", "STEM_ST:", min = 0, max = 2, value = 0.2, step = 0.01),
                                       sliderInput("cdiv", "CDIV:", min = 0, max = 0.5, value = 0.3, step = 0.01),
                                       sliderInput("sdiv", "SDIV:", min = 0, max = 0.2, value = 0.05, step = 0.01),
                                       sliderInput("csdiv", "CSDIV:", min = 0, max = 1, value = 0.08, step = 0.01)
                                     ),
                                     mainPanel(width = 0)
                                   )
                                 ),
                                 
                                 tabPanel(
                                   "Soil",
                                   fluid = F,
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 12,
                                       sliderInput("c_max", "C_MAX:", min = 0, max = 600, value = 450, step = 1),
                                       sliderInput("b_soil", "B_SOIL:", min = 0.5, max = 2.5, value = 1, step = 0.01),
                                       sliderInput("b_evap", "B_EVAP:", min = 0.5, max = 2.5, value = 1, step = 0.01),
                                       sliderInput("c_min", "C_MIN:", min = 0, max = 20, value = 10, step = 0.1),
                                       sliderInput("retcap", "RETCAP:", min = 0, max = 20, value = 2, step = 0.1)
                                     ),
                                     mainPanel(width = 0)
                                   )
                                 ),
                                 tabPanel(
                                   "Snow",
                                   fluid = F,
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 12,
                                       sliderInput("ddfa", "DDFA:", min = 0, max = 10, value = 5, step = 0.1),
                                       sliderInput("tetr", "TETR:", min = -0.5, max = 4.5, value = 0, step = 0.1),
                                       sliderInput("tmel", "TMEL:", min = -3, max = 1, value = 0.0, step = 0.1)
                                     ),
                                     mainPanel(width = 0)
                                   )),
                               ),
                               actionButton("runDhrum", "Run dHRUM", class = "btn-success")
                             )
                             # fluidRow(
                             #   tableOutput("values")
                             #   
                             # )
                  
                ),
                mainPanel(width = 8,
                          fluidRow(
                            column(width = 3,
                                   selectInput("PlotType", label = "Choose a plot:",
                                               choices = c("Time series", "Model performance", "State variables", "Evapotranspiration"))
                            ),
                            column(width = 4, offset = 1,
                                   sliderInput("Period", label = "Select the time window:",
                                               min = 0,
                                               max = 10,
                                               value = 5
                                               # timeFormat = "%F",
                                               # timezone = "+0000",
                                               # animate = FALSE)
                                   )
                            ),
                            column(width = 03,
                                   div(tableOutput("values")), style = "font-size:90%",
                                   #conditionalPanel(condition = "input.PlotType == 'Flow time series' || input.PlotType == 'Model diagram'",
            
                                   downloadButton("DownloadPlot", label = "Download plot as png",
                                                  style = "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; width:170px; height:25px; font-size:95%; padding-top:2px; margin-top:10px;")
                                   #)
                                   
                            )
                          ),
                          fluidRow(
                            conditionalPanel(condition = "input.PlotType == 'Model performance'",
                                             column(width = 09,
                                                    plotOutput("plotFDC", width = "100%"),
                                                    plotOutput("plotHydrograph", width = "100%"))),
                            conditionalPanel(condition = "input.PlotType == 'Time series'",
                                             column(width = 09,
                                                    plotOutput("plotTOTR", width = "100%"),
                                                    plotOutput("plotBASF", width = "100%"),
                                                    plotOutput("plotDIRR", width = "100%"))),
                            conditionalPanel(condition = "input.PlotType == 'State variables'",
                                             column(width = 09,
                                                    plotOutput("plotSOIS", width = "100%"),
                                                    plotOutput("plotGROS", width = "100%"),
                                                    plotOutput("plotSURS", width = "100%"))),
                            conditionalPanel(condition = "input.PlotType == 'Evapotranspiration'",
                                             column(width = 09,
                                                    plotOutput("plotPET", width = "100%"),
                                                    plotOutput("plotAET", width = "100%"),
                                                    plotOutput("plotEVAC", width = "100%"),
                                                    plotOutput("plotEVAS", width = "100%"),
                                                    plotOutput("plotEVBS", width = "100%"))),
                            
                            
                          )
                  
                )
    
  )
)
