library(hydroGOF)

annual_mean_BP <- function(outSimulation) {
  # Annual Total runoff
  outSimDT <- as.data.table(outSimulation)
  Annualmean <- outSimDT[ ,':=' (MONTH=month(DTM), YEAR = year(DTM))][,.(meanTOTR =mean(TOTR)), by= .(MONTH,YEAR) ] 
  #plot(Annualmean$meanTOTR, type = "l")
  Annualmean
}

calculation_BP <- function(out) {
  Input <- readRDS("./data/BP_benchmark_LUMPED.rds")
  outBenchMark <- Input$dta
  outSimulation <- out$dta
  
  # Kling-Gupta Efficiency
  KGEout <- KGE(outSimulation$TOTR, outBenchMark$TOTR)
  KGEoutSQRT <- KGE(sqrt(outSimulation$TOTR), sqrt(outBenchMark$TOTR))
  
  # Nash-Sutcliffe Efficiency
  NSEout <- NSE(outSimulation$TOTR, outBenchMark$TOTR)
  NSEoutSQRT <- NSE(sqrt(outSimulation$TOTR), sqrt(outBenchMark$TOTR))
  # Mean Absolute Error
  MAEout <- mae(outSimulation$TOTR, outBenchMark$TOTR)
  MAEoutSQRT <- mae(sqrt(outSimulation$TOTR), sqrt(outBenchMark$TOTR))
  
  # Table
  nm <- c("NSE(Q)", "NSE(sqrt(Q))", "KGE(Q)", "KGE(sqrt(Q))", "MAE(Q)", "MAE(sqrt(Q))")
  val <- c(NSEout, NSEoutSQRT, KGEout, KGEoutSQRT, MAEout, MAEoutSQRT)
  Stat <-  data.frame(nm, val)
  names(Stat) <- c("Criterion","Qsim")
  
  Stat
}

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
  
  BP_run(pars = parsDF)
  
}
