library(hydroGOF)

annual_mean_EVA_KL <- function(outSimulation, mean_KL) {
  outSimDT <- as.data.table(outSimulation)
  AnnualmeanEVA <- outSimDT[, ':=' (MONTH = month(DTM),
                                    YEAR = year(DTM))][, .(
                                      meanAET = mean(AET),
                                      meanEVAC = mean(EVAC),
                                      meanEVAS = mean(EVAS),
                                      meanEVBS = mean(EVBS)
                                    ),
                                    by = .(MONTH, YEAR)][, ':='(YEAR = NULL, MONTH = NULL)]
  out = AnnualmeanEVA[, DTA := mean_KL$DTA]
  out
}

annual_mean_KL <- function(outSimulation) {
  # Annual Total runoff
  outSimDT <- as.data.table(outSimulation)
  Annualmean <-
    outSimDT[, ':=' (MONTH = month(DTM), YEAR = year(DTM))][, .(meanTOTR =
                                                                  mean(TOTR)), by = .(MONTH, YEAR)]
  out = Annualmean[, DTA := as.yearmon(paste(YEAR, MONTH), "%Y %m")][, ':='(YEAR = NULL, MONTH = NULL)]
  #plot(Annualmean$meanTOTR, type = "l")
  out
}

calculation_KL <- function(out, obs) {
  #Input <- readRDS("./data/KL_benchmark_LUMPED.rds")
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
  
  # Annual Total runoff
  outSimDT <- as.data.table(outSimulation)
  Annualmean <- outSimDT[ ,':=' (MONTH=month(DTM), YEAR = year(DTM))][,.(meanTOTR =mean(TOTR)), by= .(MONTH,YEAR) ] 
  #plot(Annualmean$meanTOTR, type = "l")
  
  Stat
}

KL_runDHRUM = function(params, gwStor, swStor, start_date, end_date) {
  # START put this to the environment global variables
  days=c(30,60,90,120,150,180,210,240,270,300,330,355,364)
  p_OBS=days/365.25
  # RaBP = 96# odhad Martin Hanel
  #CHMU ZHU
  QmKL = c(22, 15, 12, 10, 8.5, 6.5, 6.0, 5.0, 3.5, 3.0, 2.0, 1.0, 0.5)
  A=3.28*1000*1000# plocha KL
  RmKL = QmKL * (3600*24) / A
  
  parsDF = params
  filname2 = "./R/eleni/data/KL_1960_01_01_noDate.txt"
  TPdta = read.table(filname2)
  prec=TPdta$V1
  temp=TPdta$V2
  nHrusKL <- 1
  AreasKL <- 3.28*1000*1000
  IdsHrus <- paste0("KL",seq(1:length(AreasKL)))
  # end global variables
  rds = readRDS("./R/eleni/data/KL_benchmark_LUMPED.rds")
  new <- data.table(DTM = rds$dta$DTM, Prec = rds$dta$PREC, Temp = rds$dta$TEMP, obsTOTR = rds$dta$TOTR)
  
  filtered <- new %>%
    select(Prec, Temp, DTM, obsTOTR) %>%
    filter(between(DTM, as.Date(start_date), as.Date(end_date)))
  
  prec = filtered$Prec
  temp = filtered$Temp
  obsTOTR = filtered$obsTOTR
  
  
  KL_run = function(pars = parsDF){
    KLdhrus <- initdHruModel(nHrusKL,AreasKL,IdsHrus)
    setPTInputsToAlldHrus(KLdhrus, Prec = prec, Temp = temp, inDate = as.Date(start_date))
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
    
    return (list(FDC = simBest, dta = copy(dtaDF), outObs = obsTOTR))
  }

  
  KL_run(pars = parsDF)
  
}
