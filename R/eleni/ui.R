library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  fluidRow(
    column(3,
      titlePanel("Adjust parameter values:"),
      sliderInput("c_max", "C_MAX:", min = 0, max = 600, value = 450),
      sliderInput("ddfa", "DDFA:", min = 0, max = 10, value = 5),
      sliderInput("retcap", "RETCAP:", min = 0, max = 20, value = 2),
      sliderInput("cans_st", "CANS_ST:", min = 0, max = 5, value = 1.5, step = 0.001),
      sliderInput("stem_st", "STEM_ST:", min = 0, max = 2, value = 0.2, step = 0.001),
      sliderInput("cdiv", "CDIV:", min = 0, max = 0.5, value = 0.3, step = 0.001),
      sliderInput("sdiv", "SDIV:", min = 0, max = 0.2, value = 0.05, step = 0.001),
      sliderInput("ks", "KS:", min = 0, max = 1, value = 0.001, step = 0.0001),
      sliderInput("kf", "KF:", min = 0, max = 1, value = 0.3, step = 0.01),
      sliderInput("adiv", "ADIV:", min = 0, max = 1, value = 0.4, step = 0.01)
    ),
    # Main panel for displaying outputs ----
   column(9,
      # Output: Table summarizing the values entered ----
    fluidRow(
      column(3,
             br(),
             tableOutput("values"),
      ),
      column(2,
             br(),
             selectInput("gwStorage","Select Groundwater storage type", 
                         choices=c("LIN_RES", "LINL_RES","LINBY_RES","EXP_RES","LIN_2SE","LIN_2PA","FLEX_RES")),
      ),
      column(2,
             br(),
             selectInput("swStorage","Select Soil water storage type", 
                         choices=c("PDM", "COLLIE_V2","NEW_ZEALAND","GR4J")),
      ),
      column(2,
             br(),
             actionButton("dhrumBP", "Run dHRUM for BP", class = "btn-success")
             
      ),
      column(2,
             br(),
             actionButton("dhrumKL", "Run dHRUM for KL", class = "btn-info"))
    ),
    fluidRow(
        column(6,
               plotOutput("plotFDC"),
        ),
        column(6,
               plotOutput("plotHydrograph")
        )
    )
   )
  )
)
