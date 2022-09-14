library(cowplot)
library(ggpubr)


setwd("D:/Project/Amalie_Paper")
getwd()


source("./Rscripts/dHRUM_setup/Catchment_divides/BP_D/BP_D_FG.R")
catchment_poly <- st_read("./Rscripts/GISdata/BP_D_WGS.shp", quiet = TRUE)

# source("./Rscripts/dHRUM_setup/Catchment_divides/BP_S/BP_S_FG.R")
# catchment_poly <- st_read("./Rscripts/GISdata/BP_S_WGS.shp", quiet = TRUE)


# source("./Rscripts/dHRUM_setup/Catchment_divides/KL_D/KL_D_FG.R")
# catchment_poly <- st_read("./Rscripts/GISdata/KL_D_FGWGS.shp", quiet = TRUE)

# source("./Rscripts/dHRUM_setup/Catchment_divides/KL_S/KL_S_FG.R")
# catchment_poly <- st_read("./Rscripts/GISdata/KL_S_FG_WGS.shp", quiet = TRUE)


# Plotting FDC curve
FDC <- data.frame(cbind(p_OBS, Rm, simBest))

colors <- c("Measured" = "black", "dHRUM" = "red")
ggplot(FDC , aes(x = p_OBS)) + 
  geom_point(aes(y = Rm, color = "Measured"), size = 3) + 
  geom_point(aes(y = simBest, color = "dHRUM"), size = 3) + 
  labs(x = "P(Qm)", y = "Qm [mm/day]", title = "FDC curvs of Simulation and Observed runoff (KL_S_FG)") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text = element_text(size = 15)) + 
  scale_color_manual(values = colors)

# library(hydroGOF)
hydroGOF::mae(FDC$simBest, FDC$Rm)
hydroGOF::NSE(FDC$simBest, FDC$Rm)
hydroGOF::KGE(FDC$simBest, FDC$Rm)


# plotting
for(j in 1:length(unique(dF_t[,HruIds]))){
  
  GW_TS <- data.table(GW_list[[j]])
  HruId1 <- dF_t[HruIds==j, ]
  
  # plot of simulated and measured groundwater 
  Groundwater <- ggplot() + 
    geom_line(data = HruId1, aes(date, scale(GROS), color = "dHRUM" ), size = 0.6) +
    geom_line(data = GW_TS, aes(date, GW_level, color = "Measured"), size = 0.6) + 
    ylab("Groundwater variation [mm]") +
    theme_bw() +  theme(axis.title.x= element_blank(),
                        panel.border = element_blank(),
                        axis.text = element_text(size = 15),
                        legend.position = "bottom", 
                        legend.title = element_blank())
  
  # Precipitation 
  Precipitation <- ggplot(data = HruId1) + geom_bar(aes(x = date, y = PREC), stat="identity", col = "blue") + 
    theme_bw() + ylab("PREC [mm/day]") + scale_y_reverse() +
    theme_bw() + theme(axis.title.x= element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x= element_blank(),
                       axis.line   = element_blank(),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text = element_text(size = 15)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggtitle(paste0("Groundwater HRU ", j))
  
  
  Plot = plot_grid(Precipitation, Groundwater, ncol = 1, align = "v", rel_heights = c(1.5, 3))
  
  # ggsave(filename = paste0("KL_S_", paste0(j, '.png')), plot = Plot, path = "./Rscripts/dHRUM_setup/Catchment_divides/",
  #        width = 30, height = 15, units = 'cm')
}

ggplot(data = dF_t) +
  geom_line(aes(date, GROS)) +
  facet_wrap(~HruIds, ncol = 6)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# calculating sum of monthly fluxes and mean of storage s for visualization 

MeanInput <- dF_t[, .(m_prec = sum(PREC), m_temp = sum(TEMP)), by= .(HruIds, Month, Year)]

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

mm <- c("January","February","March", "April","May","June","July","August",
        "September","October","November","December")



# State variable: m_prec, m_temp
# dta <- MeanInput[,mean(m_prec), by =.(Month, HruIds)][, V1, by = Month]

# # Fluxes: mn_Runoff, mn_DirectRunoff, mn_BaseFlow, mn_Percolation, mn_StemFlow
# #mn_StemFlow, mn_CanopyFlow, mn_ThroughFall, mn_Melting, mn_ActualEva, mn_PotentailEva,
# #mn_CanopyEvap, mn_StemEvap, mn_BareSoilEvap
dta <- MeanFluxes[,mean(mn_DirectRunoff), by =.(Month, HruIds)][, V1, by = Month]

# # Storages: mn_CanopyStorage, mn_StemStorage, mn_InterceptionStorage, mn_SoilStorage,
# #mn_GroundwaterStorage, mn_SurfaceRetention
# dta <- MeanStorages[,mean(mn_SoilStorage), by =.(Month, HruIds)][, V1, by = Month]


for (month in mm) {
  catchment_poly[[month]] <- as.vector(dta[Month == mm[which(mm == month)]]$V1)
}


{
  Pr = list()
  Pr[[1]] <- ggplot(data = catchment_poly, aes(fill= January)) +  geom_sf() +
    scale_fill_continuous(name = "Jan [mm]",  type="viridis")

  Pr[[2]] <- ggplot(data = catchment_poly, aes(fill= February)) +  geom_sf() +
    scale_fill_continuous(name = "Feb [mm]",  type="viridis")

  Pr[[3]] <- ggplot(data = catchment_poly, aes(fill= March)) +  geom_sf() +
    scale_fill_continuous(name = "Mar [mm]",  type="viridis")

  Pr[[4]] <- ggplot(data = catchment_poly, aes(fill= April)) +  geom_sf() +
    scale_fill_continuous(name = "Apr [mm]",  type="viridis")

  Pr[[5]] <- ggplot(data = catchment_poly, aes(fill= May)) +  geom_sf() +
    scale_fill_continuous(name = "May [mm]",  type="viridis")

  Pr[[6]] <- ggplot(data = catchment_poly, aes(fill= June)) +  geom_sf() +
    scale_fill_continuous(name = "Jun [mm]",  type="viridis")

  Pr[[7]] <- ggplot(data = catchment_poly, aes(fill= July)) +  geom_sf() +
    scale_fill_continuous(name = "Jul [mm]",  type="viridis")

  Pr[[8]] <- ggplot(data = catchment_poly, aes(fill= August)) +  geom_sf() +
    scale_fill_continuous(name = "Aug [mm]",  type="viridis")

  Pr[[9]] <- ggplot(data = catchment_poly, aes(fill= September)) +  geom_sf() +
    scale_fill_continuous(name = "Sep [mm]",  type="viridis")

  Pr[[10]] <- ggplot(data = catchment_poly, aes(fill= October)) +  geom_sf() +
    scale_fill_continuous(name = "Oct [mm]",  type="viridis")

  Pr[[11]] <- ggplot(data = catchment_poly, aes(fill= November)) +  geom_sf() +
    scale_fill_continuous(name = "Nov [mm]",  type="viridis")

  Pr[[12]] <- ggplot(data = catchment_poly, aes(fill= December)) +  geom_sf() +
    scale_fill_continuous(name = "Dec [mm]",  type="viridis")
}
ggarrange(Pr[[1]], Pr[[2]], Pr[[3]],
          Pr[[4]], Pr[[5]], Pr[[6]],
          Pr[[7]], Pr[[8]], Pr[[9]],
          Pr[[10]], Pr[[11]], Pr[[12]],
          labels = "Mean Precipitation",    # The title of the plot!
          ncol = 3, nrow = 4, common.legend = FALSE)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


P_BP_D <- st_read("./Rscripts/GISdata/BP_D_WGS.shp", quiet = TRUE)
P_BP_S <- st_read("./Rscripts/GISdata/BP_S_WGS.shp", quiet = TRUE)
P_KL_D <- st_read("./Rscripts/GISdata/KL_D_FGWGS.shp", quiet = TRUE)
P_KL_S <- st_read("./Rscripts/GISdata/KL_S_FG_WGS.shp", quiet = TRUE)

C_BP_D <- read.csv2(file = "./Rscripts/dHRUM_setup/outputs/BP_D_Corr.csv", sep = ",", header = TRUE)
C_BP_S <- read.csv2(file = "./Rscripts/dHRUM_setup/outputs/BP_S_Corr.csv", sep = ",", header = TRUE)
C_KL_D <- read.csv2(file = "./Rscripts/dHRUM_setup/outputs/KL_D_Corr.csv", sep = ",", header = TRUE)
C_KL_S <- read.csv2(file = "./Rscripts/dHRUM_setup/outputs/KL_S_Corr.csv", sep = ",", header = TRUE)


P_BP_D$Corr <- as.numeric(C_BP_D$Cor)
P_BP_S$Corr <- as.numeric(C_BP_S$Cor)
P_KL_D$Corr <- as.numeric(C_KL_D$Cor)
P_KL_S$Corr <- as.numeric(C_KL_S$Cor)

P = list()
P[[1]] <-  ggplot(data = P_BP_D, aes(fill= Corr)) +  geom_sf() +
  scale_fill_continuous(name = "Corr Coeff",  type="viridis")
P[[2]] <-  ggplot(data = P_BP_S, aes(fill= Corr)) +  geom_sf() +
  scale_fill_continuous(name = "Corr Coeff",  type="viridis")
P[[3]] <-  ggplot(data = P_KL_D, aes(fill= Corr)) +  geom_sf() +
  scale_fill_continuous(name = "Corr Coeff",  type="viridis")
P[[4]] <-  ggplot(data = P_KL_S, aes(fill= Corr)) +  geom_sf() +
  scale_fill_continuous(name = "Corr Coeff",  type="viridis")
    

ggarrange(P[[1]], P[[2]], P[[3]], P[[4]],    
          ncol = 2, nrow = 2, common.legend = TRUE) + 
  theme_pubr(legend = c("bottom"))
  

# ggtitle( "Correlation Coefficient between Simulated and Observed GW") +


