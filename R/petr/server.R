library(data.table)
library(RcppDE)
library(dHRUM)
library(dygraphs)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)

source("./dHrumBP.R")
source("./dHrumKL.R")

server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    data.frame(
      Parameter = c("B_SOIL",
                    "C_MAX",
                    "B_EVAP",
                    "KS",
                    "KF",
                    "ADIV",
                    "CDIV",
                    "SDIV",
                    "CAN_ST",
                    "STEM_ST",
                    "CSDIV",
                    "TETR",
                    "DDFA",
                    "TMEL",
                    "RETCAP",
                    "CMIN",
                    "SMAX"),
      Value = as.character(c(input$b_soil,
                             input$c_max,
                             input$b_evap,
                             input$ks,
                             input$kf,
                             input$adiv,
                             input$cdiv,
                             input$sdiv,
                             input$cans_st,
                             input$stem_st,
                             input$csdiv,
                             input$tetr,
                             input$ddfa,
                             input$tmel,
                             input$retcap,
                             input$c_min,
                             round((input$b_soil * input$c_min + input$c_max) / (input$b_soil + 1),2)
                             )),
      stringsAsFactors = FALSE)
    
   
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  
  
  # df <- reactive ({
  #   data.frame(
  #     Period = c("Start Date","End Date"),
  #     Dates = as.character(c(as.Date(min(input$date_range[1L])), as.Date(max(input$date_range[2L]))))
  #     )
  #   
  # })
  # 
  # output$date_ranges <- renderTable({
  #   df()
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
      outDta$data <- BP_runDHRUM(parsDF, 
                                 "LIN_RES", 
                                 "PDM",
                                 as.character(as.Date(min(input$date_range[1L]))), 
                                 as.character(as.Date(max(input$date_range[2L]))))
      outDta$statistics <- calculation_BP(outDta$data, outDta$data$outObs)
      outDta$annualMean <- annual_mean_BP(outDta$data$dta)
      outDta$annualMeanEVA <- annual_mean_EVA_BP(outDta$data$dta, outDta$annualMean)
      outDta$FDC = FlowDurationCurveBP(outDta$data)
      
    }
    
    
    if(input$basin == "KL basin") {
      outDta$data <- KL_runDHRUM(parsDF, 
                                 "LIN_RES", 
                                 "PDM",
                                 as.character(as.Date(min(input$date_range[1L]))), 
                                 as.character(as.Date(max(input$date_range[2L]))))
      outDta$statistics <- calculation_KL(outDta$data, outDta$data$outObs)
      outDta$annualMean <- annual_mean_KL(outDta$data$dta)
      outDta$annualMeanEVA <- annual_mean_EVA_KL(outDta$data$dta, outDta$annualMean)
      outDta$FDC = FlowDurationCurveKL(outDta$data)
    }
    
    
  })
  
  output$plotFDC <- renderPlot({
    if (is.null(outDta$data)) return()

    if(input$basin == "KL basin") {
      dtaFDC <- outDta$FDC
      dtaTOTR <- data.frame(Sim= outDta$data$dta$TOTR, Obs = outDta$data$outObs)
      qp1 <- ggplot(data = dtaTOTR, aes(x = Obs)) +
        geom_point(aes(y=Sim), color = "#8f99fb")
      
      qp2 <- ggplot(data = dtaFDC, aes(x = Days)) +
        geom_point(aes(y=ObsFDC), color = "black")+
        geom_point(aes(y=SimFDC), color = "#8f99fb")
      
      qp<-grid.arrange(qp1,qp2,ncol=2,nrow=1)
      

     
    }
    
    if(input$basin == "BP basin") {
      dtaFDC <- outDta$FDC
      dtaTOTR <- data.frame(Sim= outDta$data$dta$TOTR, Obs = outDta$data$outObs)
      qp1 <- ggplot(data = dtaTOTR, aes(x = Obs)) +
        geom_point(aes(y=Sim), color = "#8f99fb")
      
      qp2 <- ggplot(data = dtaFDC, aes(x = Days)) +
        geom_point(aes(y=ObsFDC), color = "black")+
        geom_point(aes(y=SimFDC), color = "#8f99fb")
      
      qp<-grid.arrange(qp1,qp2,ncol=2,nrow=1)
 
    }
    
    return(qp)
      })
  
  output$plotHydrograph <- renderPlotly({
    
    if (is.null(outDta$data)) return()
    
    plot <- ggplot(outDta$data$dta, aes(x=DTM)) + 
      geom_line(aes(y = TOTR), color = "#FA9335") + 
      geom_line(aes(y = BASF), color = "#0088D2") +
      labs(y="Q [mm/day]", x = "Date")
    ggplotly(plot)
    
    
    
  })
  
  output$plotAnnualMean <- renderPlot({
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$annualMean, aes(x = DTA)) +
      geom_line(aes(y = meanTOTR), color = "black") +
      labs(y="Annual Mean", x = "Date")
    
  })
  
  output$plotAnnualMeanEVA <- renderPlotly({
    if (is.null(outDta$annualMeanEVA)) return()
    
    plot <- ggplot(outDta$annualMeanEVA, aes(x = DTA)) +
      geom_line(aes(y = meanEVBS), color = "#B77252") +
      geom_line(aes(y = meanEVAC), color = "#939B66") +
      geom_line(aes(y = meanEVAS), color = "#838BC2") +
      geom_line(aes(y = meanAET), color = "#F8CF2C") +
      labs(y="Depth [mm/day]", x = "Date")
    ggplotly(plot)
    
    
  })
  
  output$plotTOTR <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    # outDta$data$
    
    # fig <- plot_ly(data =outDta$data$dta, x=~DTM, y=~TOTR, type = 'scatter', mode = 'lines') 
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = TOTR), color = "#FA9335") +
      labs(y="Total Runoff [mm/day]", x = "Date")
    
  })
  
  output$plotBASF <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = BASF), color = "#0088D2") +
      labs(y="Baseflow [mm/day]", x = "Date")
    
    
  })
  
  output$plotDIRR <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = DIRR), color = "black") +
      labs(y="Direct Runoff [mm/day]", x = "Date")
    
  })
  
  output$plotSOIS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = SOIS), color = "#6868ac") +
      labs(y="Soil storage", x = "Date")
    
    
  })
  
  output$plotGROS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = GROS), color = "black") +
      labs(y="Groundwater storage", x = "Date")
  })
  
  output$plotSURS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = SURS), color = "#BB907F") +
      labs(y="Surface retention", x = "Date")
   
    
  })
  
  output$plotPET <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = PET), color = "#6868ac") +
      labs(y="Potential Evapotranspiration", x = "Date")
    
  })
  
  output$plotAET <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = AET), color = "#F8CF2C") +
      labs(y="Actual Evapotranspiration", x = "Date")
    
  })
  
  
  output$plotEVBS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = EVBS), color = "#B77252") +
      labs(y="Bare soil Evapotranspiration", x = "Date")
    
  })
  
  output$table <- renderTable(outDta$statistics)
  
  output$DownloadData <- downloadHandler(
    
    filename = function() {
      paste("example.csv", sep = "")
    },
    content = function(file) {
      write.csv(sliderValues(), file, row.names = FALSE)
    }
  )

  
}
