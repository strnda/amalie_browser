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
      Value = as.character(c(input$c_max,
                             input$ddfa,
                             input$retcap,
                             input$can_st,
                             input$stem_st,
                             input$cdiv,
                             input$sdiv,
                             input$ks,
                             input$kf,
                             input$adiv)),
      stringsAsFactors = FALSE)
    
  })
  
  ####
  # run dhrum
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}

