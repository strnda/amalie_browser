library(shiny)
library(shinythemes)
library(lubridate)
library(dygraphs)

ui <- fluidPage(
  titlePanel("Modeling"),
  windowTitle = "dHRUM",
  sidebarLayout(position = "left",
                sidebarPanel(width = 3,
                             h4("Choose a basin:"),
                             fluidRow(
                               column(width = 12, selectInput("basin", label = NULL, choices = c("BP basin","KL basin")))
                             ),
                             h4("Adjust parameters values:"),
                             fluidRow(
                               tabsetPanel(
                                 tabPanel(
                                   "Total Runoff",
                                   fluid = F,
                                       width = 12,
                                       sliderInput("ks", "KS:", min = 0, max = 1, value = 0.001, step = 0.001),
                                       sliderInput("kf", "KF:", min = 0, max = 1, value = 0.3, step = 0.01),
                                       sliderInput("adiv", "ADIV:", min = 0, max = 1, value = 0.4, step = 0.01),
                                       style = "height: 550px;",
                                 ),
                                 tabPanel(
                                   "Vegetation" ,
                                   fluid = F,
                                       width = 12,
                                       sliderInput("cans_st", "CANS_ST:", min = 0, max = 5, value = 1.5, step = 0.01),
                                       sliderInput("stem_st", "STEM_ST:", min = 0, max = 2, value = 0.2, step = 0.01),
                                       sliderInput("cdiv", "CDIV:", min = 0, max = 0.5, value = 0.3, step = 0.01),
                                       sliderInput("sdiv", "SDIV:", min = 0, max = 0.2, value = 0.05, step = 0.01),
                                       sliderInput("csdiv", "CSDIV:", min = 0, max = 1, value = 0.08, step = 0.01),
                                       style = "height: 550px;"
                                 ),
                                 
                                 tabPanel(
                                   "Soil",
                                   fluid = F,
                                       width = 12,
                                       sliderInput("c_max", "C_MAX:", min = 0, max = 600, value = 450, step = 1),
                                       sliderInput("b_soil", "B_SOIL:", min = 0.5, max = 2.5, value = 1, step = 0.01),
                                       sliderInput("b_evap", "B_EVAP:", min = 0.5, max = 2.5, value = 1, step = 0.01),
                                       sliderInput("c_min", "C_MIN:", min = 0, max = 20, value = 10, step = 0.1),
                                       sliderInput("retcap", "RETCAP:", min = 0, max = 20, value = 2, step = 0.1),
                                       style = "height: 550px;"
                                 ),
                                 tabPanel(
                                   "Snow",
                                   fluid = F,
                                       width = 12,
                                       sliderInput("ddfa", "DDFA:", min = 0, max = 10, value = 5, step = 0.1),
                                       sliderInput("tetr", "TETR:", min = -0.5, max = 4.5, value = 0, step = 0.1),
                                       sliderInput("tmel", "TMEL:", min = -3, max = 1, value = 0.0, step = 0.1),
                                       style = "height: 550px;"
                                  )
                               ),
                               style = "padding-left: 10px;",
                               actionButton("runDhrum", "Run dHRUM", class = "btn-success"),
                               
                             )
                  
                ),
                mainPanel(width = 9,
                          fluidRow(
                            column(width = 4,
                                   selectInput("PlotType", label = "Choose a plot:",
                                               choices = c("Time series", "Model performance", "State variables", "Evapotranspiration"))
                            ),
                            column(width = 5, 
                                   sliderInput("date_range", 
                                               "Choose Date Range:", 
                                               min = as.POSIXct("1960-01-01"), 
                                               max = as.POSIXct("2016-12-30"), 
                                               value = c(as.POSIXct("1960-01-01"), as.POSIXct("2016-12-30")), 
                                               step = 1,
                                               format,
                                               timeFormat = "%Y-%m-%d"
                                   ),
                                   
                            ),
                            column(width = 3,
                                   tableOutput("date_ranges")
                                   
                                   )
                          ),
                          fluidRow(
                            column(width = 9,
                                   conditionalPanel(condition = "input.PlotType == 'Model performance'",
                                                    column(width = 09,
                                                           plotOutput("plotFDC", width = "100%"),
                                                           plotlyOutput("plotHydrograph", width = "100%"),
                                                           plotlyOutput("plotAnnualMean", width = "100%"))),
                                   conditionalPanel(condition = "input.PlotType == 'Time series'",
                                                    column(width = 09,
                                                           plotlyOutput("plotTOTR", width = "100%"),
                                                           plotlyOutput("plotBASF", width = "100%"),
                                                           plotlyOutput("plotDIRR", width = "100%"))),
                                   conditionalPanel(condition = "input.PlotType == 'State variables'",
                                                    column(width = 09,
                                                           plotlyOutput("plotSOIS", width = "100%"),
                                                           plotlyOutput("plotGROS", width = "100%"),
                                                           plotlyOutput("plotSURS", width = "100%"))),
                                   conditionalPanel(condition = "input.PlotType == 'Evapotranspiration'",
                                                    column(width = 09,
                                                           plotlyOutput("plotPET", width = "100%"),
                                                           plotlyOutput("plotAET", width = "100%"),
                                                           plotlyOutput("plotEVBS", width = "100%"),
                                                           plotlyOutput("plotAnnualMeanEVA", width = "100%")))
                                   ),
                            column(width = 3,
                                   br(),
                                   tableOutput("table"),
                                   conditionalPanel(condition = "output.table",
                                                    downloadButton("DownloadData", label = "Download data as .csv",
                                                                   style = "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; width:170px; height:25px; font-size:95%; padding-top:2px; margin-top:10px;")
                                   )
                            
                            )
                          )
                  
                )
    
  )
)
