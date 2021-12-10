library(shiny)

ui <- fluidPage(
  titlePanel("Enter parameters:"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("integer", "C_MAX:", min = 0, max = 1000, value = 450),
      sliderInput("integer", "DDFA:", min = 0, max = 100, value = 5),
      sliderInput("integer", "RETCAP:", min = 0, max = 1000, value = 500),
      sliderInput("decimal", "CAN_ST:", min = 0, max = 10, value = 1.5, step = 0.001),
      sliderInput("decimal", "STEM_ST:", min = 0, max = 10, value = 0.2, step = 0.001),
      sliderInput("decimal", "CDIV:", min = 0, max = 1, value = 0.3, step = 0.001),
      sliderInput("decimal", "SDIV:", min = 0, max = 1, value = 0.05, step = 0.001),
      sliderInput("decimal", "KS:", min = 0, max = 1, value = 0.001, step = 0.0001),
      sliderInput("decimal", "KF:", min = 0, max = 1, value = 0.3, step = 0.01),
      sliderInput("decimal", "ADIV:", min = 0, max = 1, value = 0.4, step = 0.01)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Table summarizing the values entered ----
      tableOutput("values")
      
    )
  )
)
