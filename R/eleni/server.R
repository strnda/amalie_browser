server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("C_MAX",
               "DDFA",
               "RETCAP",
               "CAN_ST",
               "STEM_ST",
               "CDIV",
               "SDIV",
               "KS",
               "KF",
               "ADIV"),
      Value = as.character(c(input$integer,
                             input$integer,
                             input$integer,
                             input$decimal,
                             input$decimal,
                             input$decimal,
                             input$decimal,
                             input$decimal,
                             input$decimal,
                             input$decimal)),
      stringsAsFactors = FALSE)
    
  })
  
  ####
  # run dhrum
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}

