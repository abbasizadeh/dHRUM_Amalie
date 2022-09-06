# This script should be run after execution of main_BP_D.r in the calibration folder
library(RcppDE)
library(data.table)
library(dHRUM)
library(ggplot2)
library(ggpubr)
library(sf)
library(rstudioapi)


setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")


getwd()

dtHrus <- as.data.table(read.csv("./inputs/Soil_input_data/Forest_Geo/BP_Drainage_FG.csv"))
SoilBP <- dtHrus #[Povodi=='BP',]
NhrusBP <- nrow(SoilBP)

nHrus <- NhrusBP
Areas <- SoilBP$Area
IdsHrus <- SoilBP$FID
dhrusBP_D_FG <- initdHruModel(nHrus,Areas,IdsHrus)
setGWtypeToAlldHrus(dHRUM_ptr = dhrusBP_D_FG ,gwTypes=rep("LIN_RES", times=nHrus),hruIds=IdsHrus)
setSoilStorTypeToAlldHrus(dHRUM_ptr = dhrusBP_D_FG,soilTypes=rep("PDM",times=nHrus),hruIds=IdsHrus)
dtaDF <- as.data.table(readRDS ("./inputs/PT_intput_data/BP_D_FG_2021.rds"))
dtaDF <- dtaDF[DTM >= as.Date("2020-01-01"), ]
setPTInputsToDistdHRUM(dHRUM_ptr = dhrusBP_D_FG, dtaDF) #look at the help

source("./Catchment_divides/BP_D/Constrained_Parameters_BP_D.r")

# Checking
ParDFup1[c("SDIV", "CDIV")]
ParDFup1$SDIV + ParDFlow1$CDIV
# Calculating potential evapotranspiration
# setParsToDistdHRUM(dhrusBP_D_FG, ParDFup1, FALSE) # it should be false

Hrus=as.character(c(dtaDF$HruId))
lat = c(SoilBP$Lat)
PetType <- as.vector(rep("HAMON", length(SoilBP$Lat)))
calcPetToAllHrusDist(dHRUM_ptr = dhrusBP_D_FG, lat, PetType, HruIds = Hrus)

#============================ Optimization ============================
# FDC
dny <- c(30,60,90,120,150,180,210,240,270,300,330,355,364)
p_OBS = dny/365.25

RaBP = 96# odhad Martin Hanel
QmBP = c(26, 18, 14, 12, 10, 8.0, 7.0, 6.0, 4.5, 3.5, 2.5, 1.0, 0.5)
A=sum(SoilBP$Area)# plocha BP
RmBP = QmBP * (3600*24) / A #CHMU ZHU 4.42 v datech SoilBP 4.41   mm/day

# Setting opt parameters
parsDF <- readRDS(paste0("./outputs/SM&GW_CalibratedParams/Pars_BP_D_FG_GW_", '1'))

for (i in 2:39){
  P <- readRDS(paste0("./outputs/SM&GW_CalibratedParams/Pars_BP_D_FG_GW_", i))
  parsDF <- rbind(parsDF, P)
}


# objuctive funciton
# mae = function(myPar){
#   newmat <- as.data.frame(matrix(myPar,nrow = nHrus, ncol = nParIhru))
#   names(newmat) <- ParNams
#   setParsToDistdHRUM(dhrusBP_D_FG, newmat, FALSE)
#   
#   dta <- dHRUMrun(dHRUM_ptr = dhrusBP_D_FG)
#   
#   dF <-data.frame(dta$outDta)
#   dF$X28 <- dta$Ids
#   names(dF) <- dta$VarsNams
#   simRM=as.numeric(quantile(dF$TOTR,probs=(1-p_OBS), na.rm = TRUE))
#   mymae =as.double(sum(abs(simRM - RmBP)))
#   
# }
# 
# # DE optim
# itermaxW = 10 # 1 iteration
# decntr<-DEoptim.control(VTR = 0, strategy = 2, bs = FALSE, NP = 6240,
#                         itermax = itermaxW, CR = 0.25, F = 0.7, trace = TRUE,
#                         initialpop = NULL, storepopfrom = itermaxW + 1,
#                         storepopfreq = 1, p = 0.2, c = 0, reltol = sqrt(.Machine$double.eps),
#                         steptol = itermaxW)
# 
# u=DEoptim(lower=as.numeric(as.matrix(ParDFlow1)),
#            upper=as.numeric(as.matrix(ParDFup1)), fn=mae, control = decntr)
# 
# 
# ParBestVec <- as.numeric(u$optim$bestmem)
# ParBestDF <- as.data.frame(matrix(ParBestVec,nrow = nHrus, ncol = nParIhru))
# names(ParBestDF) <- ParNams
# 
# parsDF <- ParBestDF


# Running model using dHRUMrun
setParsToDistdHRUM(dhrusBP_D_FG, parsDF, F)
dta <- dHRUMrun(dHRUM_ptr = dhrusBP_D_FG)
dF <- data.frame(dta$outDta)
names(dF) <- dta$VarsNams
simBest=as.numeric(quantile(dF$TOTR,probs = (1-p_OBS), na.rm = TRUE))


# saveRDS(dta,file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_D_Exp.rds")

# saveRDS(ParBestDF,file ="D:/project/Setups_For_Dist_Model/outputs/SM&GW_SalibratedParams/BP_D_FG.rds")
# saveRDS(ParBestDF,file ="D:/project/Setups_For_Dist_Model/outputs/SM&GW_SalibratedParams/BP_D_FG_GW.rds")
# saveRDS(ParBestDF,file ="D:/project/Setups_For_Dist_Model/outputs/SM&GW_SalibratedParams/BP_D_FG_SM.rds")


FDC <- data.frame(cbind(p_OBS, RmBP, simBest))

colors <- c("Measured" = "black", "dHRUM" = "red")
ggplot(FDC , aes(x = p_OBS)) + 
  geom_point(aes(y = RmBP, color = "Measured"), color = "black", size = 3) + 
  geom_point(aes(y = simBest, color = "dHRUM"), size = 3) + 
  labs(x = "P(Qm)", y = "Qm [mm/day]", title = "FDC curvs of Simulation and Observed runoff (BP_D_FG)") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text = element_text(size = 15)) + 
    scale_color_manual(values = colors)

# hydroGOF::mae(FDC$simBest, FDC$p_OBS)

# library(hydroGOF)
hydroGOF::mae(FDC$simBest, FDC$RmBP)
hydroGOF::NSE(FDC$simBest, FDC$RmBP)
hydroGOF::KGE(FDC$simBest, FDC$RmBP)

#================ Plotting================
# dHRUMrunDist
dtaDist<-dHRUMrunDist(dHRUM_ptr = dhrusBP_D_FG)
dF_Dist <- data.frame(dtaDist$outDta)
names(dF_Dist) <- dta$VarsNams
dF_Dist$HruIds <- dtaDist$Ids
dF_t <- as.data.table(dF_Dist)
dF_t$dat <- as.Date(with(dF_t, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
dF_t$Month <- months(dF_t$dat)
dF_t$Year <- format(dF_t$dat,format="%y")
#names(dF_t)

GW_list <- readRDS(file ="./inputs/Soil_input_data/SoilMoist_Groundwater/GW_HRUs.rds")  


# plotting 
for(j in 1:39){
GW_TS <- data.table(GW_list[[j]])
HruId1 <- dF_t[HruIds==j, ]

p <- ggplot() + geom_line(data = HruId1, aes(dat, scale(GROS))) +
  geom_line(data = GW_TS, aes(date, GW_level), color = 'red') + 
  ggtitle(paste0('HRU ', as.character(j)))

ggsave(filename = paste0(j, '.png'), plot = p, path = "./Catchment_divides/BP_D/",
       width = 30, height = 15, units = 'cm')
}

ggplot(data = dF_t) +
  geom_line(aes(dat, GROS)) +
  facet_wrap(~HruIds, ncol = 6)


MeanInput <- dF_t[, .(m_prec = mean(PREC), m_temp = mean(TEMP)), by= .(HruIds, Month, Year)]
# head(MeanInput)
MeanFluxes <- dF_t[, .(mn_Runoff = sum(TOTR),
                       mn_DirectRunoff = sum(DIRR),
                       mn_BaseFlow = sum(BASF),
                       mn_Percolation = sum(PERC),
                       mn_StemFlow = sum(STEF),
                       mn_CanopyFlow = sum(CANF),
                       mn_ThroughFall = sum(TROF),
                       mn_Melting = sum(MELT),
                       mn_ActualEva = sum(AET),
                       mn_PotentailEva = sum(PET),
                       mn_CanopyEvap = sum(EVAC),
                       mn_StemEvap = sum(EVAS),
                       mn_BareSoilEvap = sum(EVBS)),
                   by= .(HruIds, Month, Year)]
# head(MeanFluxes)


MeanStorages <- dF_t[, .(mn_CanopyStorage = mean(CANS),
                         mn_StemStorage = mean(STES),
                         mn_InterceptionStorage = mean(INTS),
                         mn_SoilStorage = mean(SOIS),
                         mn_GroundwaterStorage = mean(GROS),
                         mn_SurfaceRetention = mean(SURS)),
                     by = .(HruIds, Month, Year)]