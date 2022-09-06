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

dtHrus <- as.data.table(read.csv("./inputs/Soil_input_data/Forest_Geo/KL_Drainage_FG.csv"))
SoilKL <- dtHrus #[Povodi=='KL',]
NhrusKL <- nrow(SoilKL)

nHrus <- NhrusKL
Areas <- SoilKL$Area
IdsHrus <- SoilKL$FID
dhrusKL_D_FG <- initdHruModel(nHrus,Areas,IdsHrus)
setGWtypeToAlldHrus(dHRUM_ptr = dhrusKL_D_FG, gwTypes=rep("LIN_RES", times=nHrus),hruIds=IdsHrus)
setSoilStorTypeToAlldHrus(dHRUM_ptr = dhrusKL_D_FG,soilTypes=rep("PDM",times=nHrus),hruIds=IdsHrus)
# dtaDF = as.data.frame(readRDS("D:/project/Setups_For_Dist_Model/inputs/PT_intput_data/dHRUMInputs_KL_Drainage_FG.rds"))
dtaDF <- as.data.table(readRDS ("./inputs/PT_intput_data/KL_D_FG_2021.rds"))
dtaDF <- dtaDF[DTM >= as.Date("2021-01-01"), ]
setPTInputsToDistdHRUM(dHRUM_ptr = dhrusKL_D_FG, dtaDF)


source("./Catchment_divides/KL_D/Constrained_Parameters_KL_D.r")


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
RmKL = QmKL * (3600*24) / A #CHMU ZHU 4.42 v datech SoilKL 4.41   mm/day


# Setting opt parameters
parsDF <- readRDS(paste0("./outputs/SM&GW_CalibratedParams/Pars_KL_D_FG_GW_", '1'))
for (i in 2:23){
  P <- readRDS(paste0("./outputs/SM&GW_CalibratedParams/Pars_KL_D_FG_GW_", i))
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
# saveRDS(ParBestDF,file ="./outputs/SM&GW_SalibratedParams/KL_D_FG_GW.rds")
# saveRDS(ParBestDF,file ="./outputs/SM&GW_SalibratedParams/KL_D_FG_SM.rds")
# saveRDS(ParBestDF,file ="./outputs/KL_D_FG_Exp.rds")



# Setting opt parameters
# parsDF <- readRDS(file ="./outputs/SM&GW_SalibratedParams/KL_D_FG.rds")
# parsDF <- readRDS(file ="./outputs/SM&GW_SalibratedParams/KL_D_FG_GW.rds")
# parsDF <- readRDS(file ="./outputs/SM&GW_SalibratedParams/KL_D_FG_SM.rds")

# parsDF <- readRDS(file ="./outputs/KL_D_FG_Exp.rds")

 # parsDFg[c(1,2,3,4,20),] < parsDFs[c(1,2,3,4,20),]


# Running model using dHRUMrun
setParsToDistdHRUM(dhrusKL_D_FG, parsDF, F)
dta<-dHRUMrun(dHRUM_ptr = dhrusKL_D_FG)
dF <- data.frame(dta$outDta)
names(dF) <- dta$VarsNams
simBest = as.numeric(quantile(dF$TOTR, probs = (1 - p_OBS), na.rm = TRUE))


# # Plotting and comparing

FDC <- data.frame(cbind(p_OBS, RmKL, simBest))

colors <- c("Measured" = "black", "dHRUM" = "red")
ggplot(FDC , aes(x = p_OBS)) + 
  geom_point(aes(y = RmKL, color = "Measured"), color = "black", size = 3) + 
  geom_point(aes(y = simBest, color = "dHRUM"), size = 3) + 
  labs(x = "P(Qm)", y = "Qm [mm/day]", title = "FDC curvs of Simulation and Observed runoff (KL_D_FG)") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text = element_text(size = 15)) + 
  scale_color_manual(values = colors)

hydroGOF::mae(FDC$simBest, FDC$RmKL)
hydroGOF::NSE(FDC$simBest, FDC$RmKL)
hydroGOF::KGE(FDC$simBest, FDC$RmKL)

#================ Plotting================
# dHRUMrunDist
dtaDist<-dHRUMrunDist(dHRUM_ptr = dhrusKL_D_FG)
dF_Dist <- data.frame(dtaDist$outDta)
names(dF_Dist) <- dta$VarsNams
dF_Dist$HruIds <- dtaDist$Ids
dF_t <- as.data.table(dF_Dist)
dF_t$dat <- as.Date(with(dF_t, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
dF_t$Month <- months(dF_t$dat)
dF_t$Year <- format(dF_t$dat,format="%y")
#names(dF_t)

GW_list <- readRDS(file ="./inputs/Soil_input_data/SoilMoist_Groundwater/GW_KL_D_HRUs.rds")  

for(j in 1:23){
  GW_TS <- data.table(GW_list[[j]])
  HruId1 <- dF_t[HruIds==j, ]
  
  p <- ggplot() + geom_line(data = HruId1, aes(dat, scale(GROS))) +
    geom_line(data = GW_TS, aes(date, GW_level), color = 'red') + 
    ggtitle(paste0('HRU ', as.character(j)))
  
  ggsave(filename = paste0(j, '.png'), plot = p, path = "./Catchment_divides/KL_D/",
         width = 30, height = 15, units = 'cm')
}

ggplot(data = dF_t) +
  geom_line(aes(dat, GROS)) +
  facet_wrap(~HruIds, ncol = 6)


MeanInput <- dF_t[, .(m_prec = mean(PREC), m_temp = mean(TEMP)), by= .(HruIds, Month, Year) ]
head(MeanInput)

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



MeanStorages <- dF_t[, .(mn_CanopyStorage = mean(CANS),
                         mn_StemStorage = mean(STES),
                         mn_InterceptionStorage = mean(INTS),
                         mn_SoilStorage = mean(SOIS),
                         mn_GroundwaterStorage = mean(GROS),
                         mn_SurfaceRetention = mean(SURS)),
                     by = .(HruIds, Month, Year)]

