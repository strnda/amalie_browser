library(data.table)
# library(RcppDE)
library(dHRUM)
# library(dygraphs)
library(dplyr)
library(ggplot2)
# library(plotly)
library(gridExtra)


server <- function(input, output) {

  source("./R/petr/dHrumBP.R")
  source("./R/petr/dHrumKL.R")
  
  # source("dHrumBP.R")
  # source("dHrumKL.R")
  
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
      # outDta$annualMean <- annual_mean_BP(outDta$data$dta)
      # outDta$annualMeanEVA <- annual_mean_EVA_BP(outDta$data$dta, outDta$annualMean)
      outDta$FDC = FlowDurationCurveBP(outDta$data)
      
    }
    
    
    if(input$basin == "KL basin") {
      outDta$data <- KL_runDHRUM(parsDF, 
                                 "LIN_RES", 
                                 "PDM",
                                 as.character(as.Date(min(input$date_range[1L]))), 
                                 as.character(as.Date(max(input$date_range[2L]))))
      outDta$statistics <- calculation_KL(outDta$data, outDta$data$outObs)
      # outDta$annualMean <- annual_mean_KL(outDta$data$dta)
      # outDta$annualMeanEVA <- annual_mean_EVA_KL(outDta$data$dta, outDta$annualMean)
      outDta$FDC = FlowDurationCurveKL(outDta$data)
    }
    
    
  })
  
  output$plotFDC <- renderPlot({
    if (is.null(outDta$data)) return()

    # if(input$basin == "KL basin") {
    #   dtaFDC <- outDta$FDC
    #   dtaTOTR <- data.frame(Sim= outDta$data$dta$TOTR, Obs = outDta$data$outObs)
    #   qp1 <- ggplot(data = dtaTOTR, aes(x = Obs)) +
    #     geom_point(aes(y=Sim), color = "#8f99fb") +
    #     theme_bw()
    #   
    #   qp2 <- ggplot(data = dtaFDC, aes(x = Days)) +
    #     geom_point(aes(y=ObsFDC), color = "black")+
    #     geom_point(aes(y=SimFDC), color = "#8f99fb") +
    #     theme_bw()
    #   
    #   qp<-grid.arrange(qp1,qp2,ncol=2,nrow=1)
    #   
    # 
    #  
    # }
    # 
    # if(input$basin == "BP basin") {
      dtaFDC <- outDta$FDC
      dtaTOTR <- data.frame(Sim= outDta$data$dta$TOTR, Obs = outDta$data$outObs)
      qp1 <- ggplot(data = dtaTOTR, aes(x = Obs)) +
        geom_point(aes(y=Sim), color = "#8f99fb") +
        theme_bw()
      
      qp2 <- ggplot(data = dtaFDC, aes(x = Days)) +
        geom_point(aes(y=ObsFDC), color = "black")+
        geom_point(aes(y=SimFDC), color = "#8f99fb") +
        theme_bw()
      
      qp<-grid.arrange(qp1,qp2,ncol=2,nrow=1)
 
    # }
    
    return(qp)
      })
  
  # output$plotHydrograph <- renderPlotly({
  #   
  #   if (is.null(outDta$data)) return()
  #   
  #   plot <- ggplot(outDta$data$dta, aes(x=DTM)) + 
  #     geom_line(aes(y = TOTR), color = "#FA9335") + 
  #     geom_line(aes(y = BASF), color = "#0088D2") +
  #     labs(y="Q [mm/day]", x = "Date")
  #   ggplotly(plot)
  #   
  #   
  # })
  # 
  output$plotAnnualMean <- renderPlot({
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$annualMean, aes(x = dta)) +
      geom_line(aes(y = meanTOTR), color = "black") +
      labs(y="Annual Mean", x = "Date")
    
  })
  
  # output$plotAnnualMeanEVA <- renderPlotly({
  #   if (is.null(outDta$annualMeanEVA)) return()
  #   
  #   plot <- ggplot(outDta$annualMeanEVA, aes(x = DTA)) +
  #     geom_line(aes(y = meanEVBS), color = "#B77252") +
  #     geom_line(aes(y = meanEVAC), color = "#939B66") +
  #     geom_line(aes(y = meanEVAS), color = "#838BC2") +
  #     geom_line(aes(y = meanAET), color = "#F8CF2C") +
  #     labs(y="Depth [mm/day]", x = "Date")
  #   ggplotly(plot)
  #   
  #   
  # })
  
  output$plotPQ <- renderPlot({
    if (is.null(outDta$data)) return()
    
    
    g1 <- ggplot(outDta$data$dta, aes(DTM, PREC)) +
      geom_line(col="blue") +
      theme_bw() +
      ylab("PREC [mm/day]") +

      scale_y_reverse()+
      theme_bw() +
      theme(axis.title.x    = element_blank(),
            axis.text.x     = element_blank(),
            axis.ticks.x    = element_blank(),
            axis.line = element_blank()
            )+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()
      )
    
    g2 <- ggplot(outDta$data$dta, aes(DTM, TOTR))+
      geom_line(col='#8f99fb') +
      ylab("TOTR [mm/day]") +
      theme_bw() +
      # theme(legend.position = c(0.8, 0.8),
      #       legend.title    = element_blank())+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    
    g1 <- ggplot_gtable(ggplot_build(g1))
    g2 <- ggplot_gtable(ggplot_build(g2))
    qqp<- grid.arrange(g1, g2, ncol = 1, heights = c(1.7, 3))

    
    return(qqp)
    
  })

  output$plotPQsimobs <- renderPlot({
    if (is.null(outDta$data)) return()
    
    DT = data.table(outDta$data$dta)
    DT[,Obs :=outDta$data$outObs]
    outDta$data$outObs
    
    
    g1 <- ggplot(DT, aes(DTM, PREC)) +
      geom_line(col="blue") +
      ylab("PREC [mm/day]") +
      scale_y_reverse()+
      theme_bw() +
      theme(axis.title.x    = element_blank(),
            axis.text.x     = element_blank(),
            axis.ticks.x    = element_blank(),
            axis.line = element_blank()
      )+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()
      )
    colors <- c('Obs' = '#033E3E', 'Sim' = '#8f99fb')
    g2 <- ggplot(DT, aes(x=DTM))+
      geom_line(aes(y= TOTR, color="Sim"))+
      geom_line(aes(y=Obs, color="Obs"))+
      ylab("TOTR [mm/day]") +
      scale_color_manual(values = colors)+
      theme_bw() +
      theme(legend.position = c(0.9, 0.9),
            legend.title    = element_blank())+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    
    g1 <- ggplot_gtable(ggplot_build(g1))
    g2 <- ggplot_gtable(ggplot_build(g2))
    qqp<- grid.arrange(g1, g2, ncol = 1, heights = c(1.7, 3))
    
    
    return(qqp)
    
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
    DT=copy(outDta$data$dta)
    
    ggplot(data = DT, aes(x = DTM)) +
      # geom_area( fill="#6868ac", alpha=0.4) +
      geom_line(aes(y=SOIS), color="#6868ac", size=0.7) +
      # geom_point(size=0.5, color="#6868ac") +
      theme_bw() +
      labs(y="Soil storage [mm]", x = "Date")+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    
    
  })
  
  output$plotGROS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    DT=copy(outDta$data$dta)
    
    ggplot(data = DT, aes(x = DTM)) +
      # geom_area( fill="#2B3856", alpha=0.4) +
      geom_line(aes(y = GROS),color="#2B3856", size=0.7) +
      # geom_point(size=0.5, color="#2B3856") +
      theme_bw() +
      labs(y="Groundwater storage [mm]", x = "Date")+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
  })
  
  output$plotSURS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    DT=copy(outDta$data$dta)
    
    ggplot(data = DT, aes(x = DTM)) +
      # geom_area( fill="#87AFC7", alpha=0.4) +
      geom_line(aes(y = SURS),color="#87AFC7", size=0.7) +
      # geom_point(size=0.5, color="#87AFC7") +
      theme_bw() +
      labs(y="Surface retention [mm]", x = "Date")+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
   
    
  })
  
  output$plotPET <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = PET), color = "#6868ac") +
      labs(y="Potential Evapotranspiration [mm/day]", x = "Date")+
      theme_bw() +
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    
  })
  
  output$plotAET <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = AET), color = "#F8CF2C") +
      labs(y="Actual Evapotranspiration  [mm/day]", x = "Date")+
      theme_bw() +
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    
  })
  
  
  output$plotEVBS <- renderPlot({
    
    if (is.null(outDta$data)) return()
    
    ggplot(outDta$data$dta, aes(x = DTM)) +
      geom_line(aes(y = EVBS), color = "#B77252") +
      labs(y="Soil Evaporation and Transpiration  [mm/day]", x = "Date")+
      theme_bw() +
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    
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
