library(data.table)
library(RcppDE)
library(dHRUM)

BP_runDHRUM <- function(params, gwStor, swStor) {
  # START put this to the environment global variables
  days=c(30,60,90,120,150,180,210,240,270,300,330,355,364)
  p_OBS=days/365.25
  # RaBP = 96# odhad Martin Hanel
  QmBP = c(26, 18, 14, 12, 10, 8.0, 7.0, 6.0, 4.5, 3.5, 2.5, 1.0, 0.5)#l/s in 1 day
  A=4.7*1000*1000# plocha BP
  RmBP = QmBP * (3600*24) / A #CHMU ZHU mm/day
  parsDF = params
  filname2 = "./data/BP_1960_01_01.txt"
  TPdta = read.table(filname2)
  prec=TPdta$V2
  temp=TPdta$V1
  nHrusBP <- 1
  AreasBP <- 4.7*1000*1000
  IdsHrus <- paste0("BP",seq(1:length(AreasBP)))
  # end global variables
  
  
  BP_run = function(pars = parsDF){
    BPdhrus <- initdHruModel(nHrusBP,AreasBP,IdsHrus)
    setPTInputsToAlldHrus(BPdhrus, Prec = prec, Temp = temp, inDate = as.Date("1960/01/01"))
    calcPetToAllHrus(dHRUM_ptr = BPdhrus,50.1,"HAMON")
    setGWtypeToAlldHrus(dHRUM_ptr = BPdhrus ,gwTypes=rep(gwStor, times=1),hruIds=IdsHrus)
    setSoilStorTypeToAlldHrus(dHRUM_ptr = BPdhrus,soilTypes=rep(swStor,times= 1),hruIds=IdsHrus)
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
  
  BP_run(pars = parsDF);

}

KL_runDHRUM = function(params, gwStor, swStor) {
  # START put this to the environment global variables
  days=c(30,60,90,120,150,180,210,240,270,300,330,355,364)
  p_OBS=days/365.25
  # RaBP = 96# odhad Martin Hanel
  #CHMU ZHU
  QmKL = c(22, 15, 12, 10, 8.5, 6.5, 6.0, 5.0, 3.5, 3.0, 2.0, 1.0, 0.5)
  A=3.28*1000*1000# plocha KL
  RmKL = QmKL * (3600*24) / A
  
  parsDF = params
  filname2 = "./data/KL_1960_01_01_noDate.txt"
  TPdta = read.table(filname2)
  prec=TPdta$V1
  temp=TPdta$V2
  nHrusKL <- 1
  AreasKL <- 3.28*1000*1000
  IdsHrus <- paste0("KL",seq(1:length(AreasKL)))
  # end global variables
  
  
  KL_run = function(pars = parsDF){
    KLdhrus <- initdHruModel(nHrusKL,AreasKL,IdsHrus)
    setPTInputsToAlldHrus(KLdhrus, Prec = prec, Temp = temp, inDate = as.Date("1960/01/01"))
    calcPetToAllHrus(dHRUM_ptr = KLdhrus,50.1,"HAMON")
    setGWtypeToAlldHrus(dHRUM_ptr = KLdhrus ,gwTypes=rep(gwStor, times=1),hruIds=IdsHrus)
    setSoilStorTypeToAlldHrus(dHRUM_ptr = KLdhrus,soilTypes=rep(swStor,times= 1),hruIds=IdsHrus)
    setParsToDistdHRUM(KLdhrus, pars, F)
    # setParsToDistdHRUM(BPdhrus, ParBest, F)
    dta<-dHRUMrun(dHRUM_ptr = KLdhrus)
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
  
  KL_run(pars = parsDF)
}

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
                             input$ks,
                             input$kf,
                             input$adiv)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  # observe({
  #   print(input$gwStorage)
  #   print(input$swStorage)
  # })
  
  #output data from dHRUM
  outDta <- reactiveValues(data = NULL)
  
  observeEvent(input$dhrumBP, {
    
    parsDF = data.table( B_SOIL = 1,
                         C_MAX = input$c_max,
                         B_EVAP = 1,
                         KS = input$ks,
                         KF = input$kf,
                         ADIV = input$adiv,
                         CDIV = input$cdiv,
                         SDIV = input$sdiv,
                         CAN_ST = input$cans_st,
                         STEM_ST = input$stem_st,
                         CSDIV = 0.08,
                         TETR = 0,
                         DDFA = input$ddfa,
                         TMEL = 0.0,
                         RETCAP = input$retcap,
                         CMIN =10)
    outDta$data <- BP_runDHRUM(parsDF, input$gwStorage, input$swStorage)
  })
  
  observeEvent(input$dhrumKL, {
    
    parsDF = data.table( B_SOIL = 1,
                         C_MAX = input$c_max,
                         B_EVAP = 1,
                         KS = input$ks,
                         KF = input$kf,
                         ADIV = input$adiv,
                         CDIV = input$cdiv,
                         SDIV = input$sdiv,
                         CAN_ST = input$cans_st,
                         STEM_ST = input$stem_st,
                         CSDIV = 0.08,
                         TETR = 0,
                         DDFA = input$ddfa,
                         TMEL = 0.0,
                         RETCAP = input$retcap,
                         CMIN =10)
    
    outDta$data <- KL_runDHRUM(parsDF, input$gwStorage, input$swStorage)
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

