library(data.table)
library(ggplot2)
library(RcppDE)
library(dHRUM)
library(dplyr)
library(hydroGOF)

library(cowplot)

getwd()


# number of HRUs
nHrus <- 38


# Defining upper and lower bound for Params 
source("./Rscripts/dHRUM_setup/Catchment_divides/BP_S/Calibration/Constrained_Parameters_BP_S.r")

# Area; Lat; 
dtHrus <- as.data.table(read.csv("./Rscripts/dHRUM_setup/inputs/Soil_input_data/Forest_Geo/BP_Soil_FG.csv"))

# Input data
dtaDF <- as.data.table(readRDS ("./Rscripts/dHRUM_setup/inputs/PT_intput_data/BP_S_FG_2021.rds"))#   dtaDF <- dtaDF[DTM >= as.Date("2020-01-01"), ]
dtaDF_main <- dtaDF[DTM >= as.Date("2020-01-01"), ]
   

# Calibrating model for GW 
source("./Rscripts/dHRUM_setup/Catchment_divides/BP_S/Calibration/BP_S_GWoptim.r")
# ParBestGW <- readRDS(paste0("D:/project/Setups_For_Dist_Model/outputs/SM&GW_SalibratedParams/Pars_BP_S_FG_GW_", i))


# Running dHRUM using calculated parameters
GW_list <- readRDS(file ="./Rscripts/dHRUM_setup/inputs/Soil_input_data/SoilMoist_Groundwater/GW_BP_S_HRUs.rds")  
Mtr_dF <- data.frame()
for (i in 1:38){
  ParBestDF <- readRDS(paste0("./Rscripts/dHRUM_setup/outputs/SM&GW_CalibratedParams/Pars_BP_S_FG_GW_", i))
  

  SoilBP <- dtHrus[FID == i,]
  NhrusBP <- nrow(SoilBP)
  nHrusBP <- NhrusBP # Change NhrusBP to nhursBP to be able to run Constrained_Parameters.r file
  
  Areas <- SoilBP$Area
  IdsHrus <- SoilBP$FID
  
  dhrusBP_D_FG <- initdHruModel(nHrusBP, Areas, IdsHrus)
  setGWtypeToAlldHrus(dHRUM_ptr = dhrusBP_D_FG, gwTypes = rep("LIN_RES", times=nHrusBP), hruIds = IdsHrus)
  setSoilStorTypeToAlldHrus(dHRUM_ptr = dhrusBP_D_FG, soilTypes = rep("PDM",times=nHrusBP), hruIds = IdsHrus)
  dtaDF <- dtaDF_main[HruId == i, ]
  
  setPTInputsToDistdHRUM(dHRUM_ptr = dhrusBP_D_FG, dtaDF) 
  
  Hrus=as.character(c(dtaDF$HruId))
  lat = c(SoilBP$Lat)
  PetType <- as.vector(rep("HAMON", length(SoilBP$Lat)))
  calcPetToAllHrusDist(dHRUM_ptr = dhrusBP_D_FG, lat, PetType, HruIds = Hrus)
  
  # calcPetToAllHrusDist(dHRUM_ptr = dhrusBP_D_FG, lat, PetType, HruIds = i) # HruIds = Hrus
  
  setParsToDistdHRUM(dhrusBP_D_FG, ParBestDF, FALSE)
  dtaDist <- dHRUMrunDist(dHRUM_ptr = dhrusBP_D_FG)
  
  dF_t <- data.table(dtaDist$outDta)
  dF_t$X28 <- dtaDist$Ids
  names(dF_t) <- dtaDist$VarsNams
  dF_t$HruIds <- dtaDist$Ids
  dF_t$date <- as.Date(with(dF_t, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  dF_t$Month <- months(dF_t$date)
  dF_t$Year <- format(dF_t$date,format="%y")
  
  
  GW_TS <- data.table(GW_list[[i]])
  rngGW <- range(GW_TS[, date])
  
  # ggplot() + geom_line(data = dF_t, aes(date, scale(GROS))) +
  #   geom_line(data = GW_TS, aes(date, GW_level), color = 'red') +
  #   ggtitle(paste0('HRU ', as.character(i)))

#------------------------- Plotting --------------------------------

  dF_t <- as.data.table(dF_t)
  GWrange_dHRUM  <- dF_t[, .(date, GROS)]
  GWrange_dHRUM <- GWrange_dHRUM[, stdGROS := scale(GROS)]
  GWrange_dHRUM  <- GWrange_dHRUM[date >= rngGW[1] & date <= rngGW[2], .(date, stdGROS)]
  
  GW <- merge(GW_TS[HruId == i, ], GWrange_dHRUM, by = "date")
  names(dtaDF) <- c('date', 'HruId', 'P', 'T')
  GW <- merge(GW, dtaDF, by = "date")
  GW$HruId.y <- NULL
  names(GW) <- c('date', 'GW_level', 'HruId', 'stdGROS', 'P', 'T')
  
  Mtr_dF[i, 1] <- cor(GW[, GW_level], GW[, stdGROS])
  Mtr_dF[i, 2] <- hydroGOF::NSE(GW[, stdGROS], GW[, GW_level])
  Mtr_dF[i, 3] <- hydroGOF::KGE(GW[, stdGROS], GW [, GW_level])
# sort(unique(GW_TS$HruId))
}
names(Mtr_dF) <- c('Cor', 'GOF', 'KGE')
Mtr_dF

colors <- c("Measured" = "black", "dHRUM" = "red")

P1 <- ggplot(GW, aes(x = date)) + 
  geom_line(aes(y = GW_level, color = "Measured" ), size = 1) +
  geom_line(aes(y = stdGROS, color = "dHRUM"), size = 1) + 
  labs(x = "Date", y = "Groundwater Level Fluctuation", color = "Legend") + 
  theme_bw() +
  scale_color_manual(values = colors) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position ="bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 15),
    legend.text = element_text(size=15)) 
  

P2 <- ggplot(GW) + geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + theme_bw() +
  ylab("PREC [mm/day]") +
  scale_y_reverse() +
  theme_bw() +
  theme(axis.title.x= element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x= element_blank(),
        axis.line   = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  ggtitle(paste0("Groundwater HRU ", i))#, OF))


plot_grid(P2, P1, ncol = 1, align = "v", rel_heights = c(1.5, 3))
