# library(hydroGOF)
library(data.table)
library(dplyr)

FlowDurationCurveBP<-function(outdta){
  return(outdta$FDC)
}


annual_mean_EVA_BP <- function(outSimulation, mean_BP) {
  outSimDT <- as.data.table(outSimulation)
  AnnualmeanEVA <- outSimDT[, ':=' (MONTH = month(DTM),
                                    YEAR = year(DTM))][, .(
                                      meanAET = mean(AET),
                                      meanEVAC = mean(EVAC),
                                      meanEVAS = mean(EVAS),
                                      meanEVBS = mean(EVBS)
                                    ),
                                    by = .(MONTH, YEAR)][, ':='(YEAR = NULL, MONTH = NULL)]
  out = AnnualmeanEVA[, DTA := mean_BP$DTA]
  out
}

annual_mean_BP <- function(outSimulation) {
  # Annual Total runoff
  outSimDT <- as.data.table(outSimulation)
  Annualmean <-
    outSimDT[, ':=' (MONTH = month(DTM), YEAR = year(DTM))][, .(meanTOTR =
                                                                  mean(TOTR)), by = .(MONTH, YEAR)]
  out = Annualmean[, DTA := as.yearmon(paste(YEAR, MONTH), "%Y %m")][, ':='(YEAR = NULL, MONTH = NULL)]
  #plot(Annualmean$meanTOTR, type = "l")
  out
}

KGE <- function(obs,sim){
  
  r=cor(obs,sim)
  sdfr= sd(sim) / sd(obs)
  mufr = mean(sim) / mean(obs)
  
  return( 1-((r-1)^2 + (sdfr-1)^2+ (mufr-1)^2)^0.5)
}

NSE <- function(obs,sim){
  
  ff=sum((obs-sim)^2)
  fo=sum((obs-mean(obs))^2)
  
  return( 1-ff/fo)
}

mae <- function(obs, sim){
  
  return(mean(abs(sim-obs)))
}


calculation_BP <- function(out, obs) {
  #Input <- readRDS("./data/BP_benchmark_LUMPED.rds")
  #outBenchMark <- Input$dta
  outSimulation <- out$dta
  
  # Kling-Gupta Efficiency
  KGEout <- KGE(outSimulation$TOTR, obs)
  KGEoutSQRT <- KGE(sqrt(outSimulation$TOTR), sqrt(obs))
  
  # Nash-Sutcliffe Efficiency
  NSEout <- NSE(outSimulation$TOTR, obs)
  NSEoutSQRT <- NSE(sqrt(outSimulation$TOTR), sqrt(obs))
  # Mean Absolute Error
  MAEout <- mae(outSimulation$TOTR, obs)
  MAEoutSQRT <- mae(sqrt(outSimulation$TOTR), sqrt(obs))
  
  # Table
  nm <- c("NSE(Q)", "NSE(sqrt(Q))", "KGE(Q)", "KGE(sqrt(Q))", "MAE(Q)", "MAE(sqrt(Q))")
  val <- c(NSEout, NSEoutSQRT, KGEout, KGEoutSQRT, MAEout, MAEoutSQRT)
  Stat <-  data.frame(nm, val)
  names(Stat) <- c("Criterion","Qsim")
  
  Stat
}

BP_runDHRUM <- function(params, gwStor, swStor, start_date, end_date) {
  # START put this to the environment global variables
  days=c(30,60,90,120,150,180,210,240,270,300,330,355,364)
  p_OBS=days/365.25
  # RaBP = 96# odhad Martin Hanel
  QmBP = c(26, 18, 14, 12, 10, 8.0, 7.0, 6.0, 4.5, 3.5, 2.5, 1.0, 0.5)#l/s in 1 day
  A=4.7*1000*1000# plocha BP
  RmBP = QmBP * (3600*24) / A #CHMU ZHU mm/day
  parsDF = params
  # setwd("/home/eleni/CULS_FES/semester_3/Presentation of Environmental Data/project/dHRUM_shiny/R/eleni/data/")
  # filname2 = "BP_1960_01_01.txt"
  # TPdta = read.table(filname2)
  # prec=TPdta$V2
  # temp=TPdta$V1
  # dtaPT <- data.table(Prec = prec, Temp = temp)
  #sequence of dates
  nHrusBP <- 1
  AreasBP <- 4.7*1000*1000
  IdsHrus <- paste0("BP",seq(1:length(AreasBP)))
  # end global variables
  #DTM = seq(from = as.Date("1960-01-01"), to = as.Date("2020-12-31"), by = 'day')
  
  #new <- data.table(Prec = prec, Temp = temp, DTM = DTM)
  #setwd("/home/eleni/CULS_FES/semester_3/Presentation of Environmental Data/project/dHRUM_shiny/R/eleni/data/")
  rds = readRDS("./R/petr/data/BP_benchmark_LUMPED.rds")
  
  new <- data.table(DTM = rds$dta$DTM, Prec = rds$dta$PREC, Temp = rds$dta$TEMP, obsTOTR = rds$dta$TOTR)
  
  filtered <- new %>%
    select(Prec, Temp, DTM, obsTOTR) %>%
    filter(between(DTM, as.Date(start_date), as.Date(end_date)))
  
  prec = filtered$Prec
  temp = filtered$Temp
  obsTOTR = filtered$obsTOTR
  
  
  BP_run = function(pars = parsDF){
    BPdhrus <- initdHruModel(nHrusBP,AreasBP,IdsHrus)
    setPTInputsToAlldHrus(BPdhrus, Prec = prec, Temp = temp, inDate = as.Date(start_date))
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
    FDCdta = data.frame(Days = days, ObsFDC=RmBP, SimFDC = simBest)
    
    return (list(dta = copy(dtaDF), outObs = obsTOTR,FDC = FDCdta))
  }
  
  return(BP_run(pars = parsDF))
  
}
