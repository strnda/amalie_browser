library(shiny)

ui <- fluidPage(
  titlePanel("Enter parameters:"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("c_max", "C_MAX:", min = 0, max = 1000, value = 450),
      sliderInput("ddfa", "DDFA:", min = 0, max = 1000, value = 5),
      sliderInput("retcap", "RETCAP:", min = 0, max = 1000, value = 500),
      sliderInput("can_st", "CAN_ST:", min = 0, max = 10, value = 1.5, step = 0.001),
      sliderInput("stem_st", "STEM_ST:", min = 0, max = 10, value = 0.2, step = 0.001),
      sliderInput("cdiv", "CDIV:", min = 0, max = 1, value = 0.3, step = 0.001),
      sliderInput("sdiv", "SDIV:", min = 0, max = 1, value = 0.05, step = 0.001),
      sliderInput("ks", "KS:", min = 0, max = 1, value = 0.001, step = 0.0001),
      sliderInput("kf", "KF:", min = 0, max = 1, value = 0.3, step = 0.01),
      sliderInput("adiv", "ADIV:", min = 0, max = 1, value = 0.4, step = 0.01)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Table summarizing the values entered ----
      tableOutput("values")
      
    )
  )
)
