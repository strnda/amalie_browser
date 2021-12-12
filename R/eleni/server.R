library(data.table)
library(RcppDE)
library(dHRUM)

source("dHrumBP.R")
source("dHrumKL.R")

server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    data.frame(
      Parameter = c("C_MAX",
                    "DDFA",
                    "RETCAP",
                    "CANS_ST",
                    "STEM_ST",
                    "CDIV",
                    "SDIV",
                    "CSDIV",
                    "KS",
                    "KF",
                    "ADIV"),
      Value = as.character(c(input$c_max,
                             input$ddfa,
                             input$retcap,
                             input$cans_st,
                             input$stem_st,
                             input$cdiv,
                             input$sdiv,
                             input$csdiv,
                             input$ks,
                             input$kf,
                             input$adiv)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  # output$values <- renderTable({
  #   sliderValues()
  # })
  
  outDta <- reactiveValues(data = NULL)
  
  observeEvent(input$runDhrum, {
    
    parsDF = data.table( B_SOIL = input$b_soil,
                         C_MAX = input$c_max,
                         B_EVAP = input$b_evap,
                         KS = input$ks,
                         KF = input$kf,
                         ADIV = input$adiv,
                         CDIV = input$cdiv,
                         SDIV = input$sdiv,
                         CAN_ST = input$cans_st,
                         STEM_ST = input$stem_st,
                         CSDIV = input$csdiv,
                         TETR = input$tetr,
                         DDFA = input$ddfa,
                         TMEL = input$tmel,
                         RETCAP = input$retcap,
                         CMIN = input$c_min
                         )
    if(input$basin == "BP basin") {
      outDta$data <- BP_runDHRUM(parsDF, "LIN_RES", "PDM")
    }
    
    if(input$basin == "KL basin") {
      outDta$data <- KL_runDHRUM(parsDF, "LIN_RES", "PDM")
    }
    
    
  })
  
  
  output$plotFDC <- renderPlot({
    if (is.null(outDta$data)) return()
    
    plot = plot(days,RmBP, 
                pch = 19, 
                ylim = range(c(outDta$data$FDC,RmBP)), 
                ylab ="Qm [mm/den]", 
                xlab="Day")
    plot = plot + points(days, 
                         outDta$data$FDC, 
                         col="red", 
                         pch=19)
    grid()
    
  })
  output$plotHydrograph <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$TOTR,
                type = "l", xlab = "Date", 
                ylab="Q [mm/den]")
    plot = plot + lines(outDta$data$dta$DTM,
                        outDta$data$dta$BASF,
                        col='red')
    grid()
  })
  
}

