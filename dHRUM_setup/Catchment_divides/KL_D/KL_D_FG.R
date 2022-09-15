library(RcppDE)
library(data.table)
library(dHRUM)
library(ggplot2)
library(ggpubr)
library(sf)

getwd()

dtHrus <- as.data.table(read.csv("./Rscripts/dHRUM_setup/inputs/Soil_input_data/Forest_Geo/KL_Drainage_FG.csv"))
SoilKL <- dtHrus #[Povodi=='KL',]
NhrusKL <- nrow(SoilKL)

nHrus <- NhrusKL
Areas <- SoilKL$Area
IdsHrus <- SoilKL$FID
dhrusKL_D_FG <- initdHruModel(nHrus,Areas,IdsHrus)
setGWtypeToAlldHrus(dHRUM_ptr = dhrusKL_D_FG, gwTypes=rep("LIN_RES", times=nHrus),hruIds=IdsHrus)
setSoilStorTypeToAlldHrus(dHRUM_ptr = dhrusKL_D_FG,soilTypes=rep("PDM",times=nHrus),hruIds=IdsHrus)
# dtaDF = as.data.frame(readRDS("D:/project/Setups_For_Dist_Model/inputs/PT_intput_data/dHRUMInputs_KL_Drainage_FG.rds"))
dtaDF <- as.data.table(readRDS ("./Rscripts/dHRUM_setup/inputs/PT_intput_data/KL_D_FG_2021.rds"))
dtaDF <- dtaDF[DTM >= as.Date("2021-01-01"), ]
setPTInputsToDistdHRUM(dHRUM_ptr = dhrusKL_D_FG, dtaDF)


# source("./Rscripts/dHRUM_setup/Catchment_divides/KL_D/Calibration/Constrained_Parameters_KL_D.r")


# Calculating potential evapotranspiration
Hrus=as.character(c(dtaDF$HruId))
lat = c(SoilKL$Lat)
PetType <- as.vector(rep("HAMON", length(SoilKL$Lat)))
calcPetToAllHrusDist(dHRUM_ptr = dhrusKL_D_FG, lat, PetType, HruIds = Hrus)

#============================ Optimization ============================
# FDC
dny <- c(30,60,90,120,150,180,210,240,270,300,330,355,364)
p_OBS = dny/365.25

RaKL = 96# odhad Martin Hanel
QmKL = c(22, 15, 12, 10, 8.5, 6.5, 6.0, 5.0, 3.5, 3.0, 2.0, 1.0, 0.5)
A=sum(SoilKL$Area)# plocha KL
Rm = QmKL * (3600*24) / A #CHMU ZHU 4.42 v datech SoilKL 4.41   mm/day


# Setting opt parameters
parsDF <- readRDS(paste0("./Rscripts/dHRUM_setup/outputs/SM&GW_CalibratedParams/Pars_KL_D_FG_GW_", '1'))
for (i in 2:23){
  P <- readRDS(paste0("./Rscripts/dHRUM_setup/outputs/SM&GW_CalibratedParams/Pars_KL_D_FG_GW_", i))
  parsDF <- rbind(parsDF, P)
}

# # objuctive funciton
# mae = function(myPar){
#   newmat <- as.data.frame(matrix(myPar,nrow = nHrus, ncol = nParIhru))
#   names(newmat) <- ParNams
#   setParsToDistdHRUM(dhrusKL_D_FG, newmat, FALSE)
# 
#   dta <- dHRUMrun(dHRUM_ptr = dhrusKL_D_FG)
# 
#   dF <-data.frame(dta$outDta)
#   dF$X28 <- dta$Ids
#   names(dF) <- dta$VarsNams
#   simRM=as.numeric(quantile(dF$TOTR,probs=(1-p_OBS), na.rm = TRUE))
#   mymae =as.double(sum(abs(simRM - RmKL)))
# 
# }
# 
# # DE optim
# itermaxW = 10 # 1 iteration
# decntr<-DEoptim.control(VTR = 0, strategy = 2, bs = FALSE, NP = 3680,
#                         itermax = itermaxW, CR = 0.25, F = 0.7, trace = TRUE,
#                         initialpop = NULL, storepopfrom = itermaxW + 1,
#                         storepopfreq = 1, p = 0.2, c = 0, reltol = sqrt(.Machine$double.eps),
#                         steptol = itermaxW)
# 
# u=DEoptim( lower=as.numeric(as.matrix(ParDFlow1)),
#            upper=as.numeric(as.matrix(ParDFup1)), fn=mae, control = decntr)
# 
# 
# ParBestVec <- as.numeric(u$optim$bestmem)
# ParBestDF <- as.data.frame(matrix(ParBestVec,nrow = nHrus, ncol = nParIhru))
# names(ParBestDF) <- ParNams
# 
# parsDF <- ParBestDF

# saveRDS(ParBestDF,file ="./outputs/SM&GW_SalibratedParams/KL_D_FG.rds")

# Running model using dHRUMrun
setParsToDistdHRUM(dhrusKL_D_FG, parsDF, F)
dta<-dHRUMrun(dHRUM_ptr = dhrusKL_D_FG)
dF <- data.frame(dta$outDta)
names(dF) <- dta$VarsNams
simBest = as.numeric(quantile(dF$TOTR, probs = (1 - p_OBS), na.rm = TRUE))

# save the lumped model output for producing heatmap 
# saveRDS(dta,file ="./Rscripts/dHRUM_setup/outputs/HeatMapData/KL_D_GW.rds")


#================ Plotting================
# dHRUMrunDist
dtaDist<-dHRUMrunDist(dHRUM_ptr = dhrusKL_D_FG)
dF_Dist <- data.frame(dtaDist$outDta)
names(dF_Dist) <- dta$VarsNams
dF_Dist$HruIds <- dtaDist$Ids
dF_t <- as.data.table(dF_Dist)
dF_t$date <- as.Date(with(dF_t, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
dF_t$Month <- months(dF_t$dat)
dF_t$Year <- format(dF_t$dat,format="%y")
#names(dF_t)



GW_list <- readRDS(file ="./Rscripts/dHRUM_setup/inputs/Soil_input_data/SoilMoist_Groundwater/GW_KL_D_HRUs.rds")  


