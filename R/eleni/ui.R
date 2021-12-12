library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  fluidRow(titlePanel("dHRUM Lumped Version"),
           column(4,
                  tabsetPanel(
                    tabPanel(
                      "Direct Runoff",
                      fluid = F,
                      sidebarLayout(
                        sidebarPanel(
                          width = 12,
                          sliderInput("ks", "KS:", min = 0, max = 1, value = 0.001, step = 0.0001),
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
                          sliderInput("cans_st", "CANS_ST:", min = 0, max = 5, value = 1.5, step = 0.001),
                          sliderInput("stem_st", "STEM_ST:", min = 0, max = 2, value = 0.2, step = 0.001),
                          sliderInput("cdiv", "CDIV:", min = 0, max = 0.5, value = 0.3, step = 0.001),
                          sliderInput("sdiv", "SDIV:", min = 0, max = 0.2, value = 0.05, step = 0.001),
                          sliderInput("csdiv", "CSDIV:", min = 0, max = 1, value = 0.08, step = 0.001)
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
                          sliderInput("b_soil", "B_SOIL:", min = 0.5, max = 2.5, value = 1, step = 0.001),
                          sliderInput("b_evap", "B_EVAP:", min = 0.5, max = 2.5, value = 1, step = 0.001),
                          sliderInput("c_min", "C_MIN:", min = 0, max = 20, value = 10, step = 0.01),
                          sliderInput("retcap", "RETCAP:", min = 0, max = 20, value = 2, step = 0.01)
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
                          sliderInput("ddfa", "DDFA:", min = 0, max = 10, value = 5, step = 0.001),
                          sliderInput("tetr", "TETR:", min = -0.5, max = 4.5, value = 0, step = 0.001),
                          sliderInput("tmel", "TMEL:", min = -3, max = 1, value = 0.0, step = 0.001)
                        ),
                        mainPanel(width = 0)
                      )),
                  ),
                  tableOutput("values"),
                  actionButton("runDhrum", "Run dHRUM", class = "btn-success")
  ),
  column(4,
         plotOutput("plotBP"),
         plotOutput("plotKL")
         
    )
  )
)
