library(data.table)
library(RcppDE)
library(dHRUM)
library(dygraphs)

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
  
  # output$out1 <- renderText({
  #   seq(from = input$date_range[1], to = input$date_range[1])
  #   #as.Date(input$date_range[1]):as.Date(input$date_range[2])
  # })
  
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
      outDta$statistics <- calculation_BP(outDta$data)
      outDta$annualMean <- annual_mean_BP(outDta$data$dta)
      outDta$annualMeanEVA <- annual_mean_EVA_BP(outDta$data$dta, outDta$annualMean)
      
    }
    
    if(input$basin == "KL basin") {
      outDta$data <- KL_runDHRUM(parsDF, "LIN_RES", "PDM")
      outDta$statistics <- calculation_KL(outDta$data)
      outDta$annualMean <- annual_mean_KL(outDta$data$dta)
      outDta$annualMeanEVA <- annual_mean_EVA_KL(outDta$data$dta, outDta$annualMean)
    }
    
    
  })
  
  output$plotFDC <- renderPlot({
    if (is.null(outDta$data)) return()
    
    plot = plot(days,RmBP, 
                pch = 19, 
                ylim = range(c(outDta$data$FDC,RmBP)), 
                ylab ="Qm [mm/day]", 
                xlab="Day")
    plot = plot + points(days, 
                         outDta$data$FDC, 
                         col="red", 
                         pch=19)
    
  })
  
  output$plotHydrograph <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    # ggplotly(ggplot(data = sliderValues(),
    #                 mapping = aes_string(x = outDta$data$dta$DTM, y = outDta$data$dta$TOTR)) +
    #            geom_line()) %>% layout(xaxis = list(rangeslider = list(type = "date")))
    # 
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$TOTR,
                type = "l", xlab = "Date",
                ylab="Q [mm/day]")
    plot = plot + lines(outDta$data$dta$DTM,
                        outDta$data$dta$BASF,
                        col='red')
  })
  
  output$plotAnnualMean <- renderPlot({
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$annualMean$DTA, outDta$annualMean$meanTOTR, type = "l", xlab="Year", ylab="Annual Mean")
    
  })
  
  output$plotAnnualMeanEVA <- renderDygraph({
    
    AET <- ts(frequency = 12, start = c(1960, 1),outDta$annualMeanEVA$meanAET)
    EVAC <- ts(frequency = 12, start = c(1960, 1),outDta$annualMeanEVA$meanEVAC)
    EVAS <- ts(frequency = 12, start = c(1960, 1),outDta$annualMeanEVA$meanEVAS)
    EVBS <- ts(frequency = 12, start = c(1960, 1),outDta$annualMeanEVA$meanEVBS)
    
    EVAP <- cbind(AET, EVAS, EVAC, EVBS)
    dygraph(EVAP)%>% dyAxis("y", label = "Depth [mm/dat]")  %>% 
      dyOptions(colors =c("black","blue", "green", "red"))%>%
      dyRangeSelector()
    
  })
  
  output$plotTOTR <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$TOTR, type="l", xlab="Date", ylab="Total Runoff [mm/day]")
    
  })
  
  output$plotBASF <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$BASF, type="l", xlab="Date", ylab="Baseflow [mm/day]")
  })
  
  output$plotDIRR <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$DIRR, type="l", xlab="Date", ylab="Direct Runoff [mm/day]")
  })
  
  output$plotSOIS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$SOIS, type="l", xlab="Date", ylab="Soil storage")
    
  })
  
  output$plotGROS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$GROS, type="l", xlab="Date", ylab="Groundwater storage")
  })
  
  output$plotSURS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$SURS, type="l", xlab="Date", ylab="Surface retention")
  })
  
  output$plotPET <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$PET, type="l", xlab="Date", ylab="Potential Evapotranspiration")
  })
  
  output$plotAET <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$AET, type="l", xlab="Date", ylab="Actual Evapotranspiration")
  })
  
  
  output$plotEVBS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    plot = plot(outDta$data$dta$DTM,
                outDta$data$dta$EVBS, type="l", xlab="Date", ylab="Bare soil Evapotranspiration")
  })
  
  output$table <- renderTable(outDta$statistics)
  
}
