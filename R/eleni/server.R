library(data.table)
library(RcppDE)
library(dHRUM)

server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Parameter = c("C_MAX",
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
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  ####
  # run dhrum
  
  # START put this to the environment global variables
  days=c(30,60,90,120,150,180,210,240,270,300,330,355,364)
  p_OBS=days/365.25
  # RaBP = 96# odhad Martin Hanel
  QmBP = c(26, 18, 14, 12, 10, 8.0, 7.0, 6.0, 4.5, 3.5, 2.5, 1.0, 0.5)#l/s in 1 day
  A=4.7*1000*1000# plocha BP
  RmBP = QmBP * (3600*24) / A #CHMU ZHU mm/day
  parsDF = data.table( B_SOIL = 1,
                       C_MAX = 450,
                       B_EVAP = 1,
                       KS = 0.001,
                       KF = 0.3,
                       ADIV = 0.4,
                       CDIV = 0.3,
                       SDIV = 0.05,
                       CAN_ST = 1.5,
                       STEM_ST = .2,
                       CSDIV = 0.08,
                       TETR = 0,
                       DDFA = 5,
                       TMEL = 0.0,
                       RETCAP = 2,
                       CMIN =10)
  filname2 = "~/tmp/dHRUM/dHRUM/Calibrations/Amalie/indata/KL_1960_01_01_noDate.txt"
  TPdta = read.table(filname2)
  prec=TPdta$V1
  temp=TPdta$V2
  nHrusBP <- 1
  AreasBP <- 4.7*1000*1000
  IdsHrus <- paste0("BP",seq(1:length(AreasBP)))
  # end global variables
  
  
  BP_run = function(pars = parsDF){
    BPdhrus <- initdHruModel(nHrusBP,AreasBP,IdsHrus)
    setPTInputsToAlldHrus(BPdhrus, Prec = prec, Temp = temp, inDate = as.Date("1960/01/01"))
    calcPetToAllHrus(dHRUM_ptr = BPdhrus,50.1,"HAMON")
    setGWtypeToAlldHrus(dHRUM_ptr = BPdhrus ,gwTypes=rep("LIN_RES", times=1),hruIds=IdsHrus)
    setSoilStorTypeToAlldHrus(dHRUM_ptr = BPdhrus,soilTypes=rep("PDM",times= 1),hruIds=IdsHrus)
    setParsToDistdHRUM(BPdhrus, pars, F)
    # setParsToDistdHRUM(BPdhrus, ParBest, F)
    dta<-dHRUMrun(dHRUM_ptr = BPdhrus)
    dtaDF <- as.data.frame(dta$outDta)
    names(dtaDF) <- dta$VarsNams
    dtaDF <- as.data.table(dtaDF)
    
    dtaDF[,DTM:=as.Date(paste(YEAR,MONTH,DAY,sep="-"))]
    
    dtaDF[,YEAR:=NULL]
    dtaDF[,MONTH:=NULL]
    dtaDF[,DAY:=NULL]
    dtaDF[,JDAY:=NULL]
    
    simBest=as.numeric(quantile(dtaDF$TOTR,probs=(1-p_OBS), na.rm = TRUE))
    
    return (list(FDC = simBest, dta = copy(dtaDF)))
  }
  
  out = BP_run(pars = parsDF)
  #out = BP_run(pars = ParBest)
  
  v <- reactiveValues(data = NULL)
  
  #  possible graphical output
  observeEvent(input$dhrum, {
    v$data <- out
  })
  
  output$plotFDC <- renderPlot({
    if (is.null(v$data)) return()
    plot = plot(days,RmBP, pch=19, ylim= range(c(out$FDC,RmBP)), ylab="Qm [mm/den]", xlab="Day")
    plot = plot + points(days,out$FDC,col="red",pch=19)
    grid()
  })
  
  output$plotHydrograph <- renderPlot({
    if (is.null(v$data)) return()
    plot = plot(out$dta$DTM,out$dta$TOTR, type="l", xlab="Date", ylab="Q [mm/den]")
    plot = plot + lines(out$dta$DTM,out$dta$BASF,col='red')
    grid()
  })
  
}

