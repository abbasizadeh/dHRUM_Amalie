library(cowplot)

# source("./Rscripts/dHRUM_setup/Catchment_divides/BP_D/BP_D_FG.R")
# source("./Rscripts/dHRUM_setup/Catchment_divides/BP_S/BP_S_FG.R")
source("./Rscripts/dHRUM_setup/Catchment_divides/KL_D/KL_D_FG.R")
# source("./Rscripts/dHRUM_setup/Catchment_divides/KL_S/KL_S_FG.R")


# Plotting FDC curve
FDC <- data.frame(cbind(p_OBS, Rm, simBest))

colors <- c("Measured" = "black", "dHRUM" = "red")
ggplot(FDC , aes(x = p_OBS)) + 
  geom_point(aes(y = Rm, color = "Measured"), size = 3) + 
  geom_point(aes(y = simBest, color = "dHRUM"), size = 3) + 
  labs(x = "P(Qm)", y = "Qm [mm/day]", title = "FDC curvs of Simulation and Observed runoff (BP_D_FG)") +
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
  
  # ggsave(filename = paste0(j, '.png'), plot = Plot, path = "./Rscripts/dHRUM_setup/Catchment_divides/",
  #        width = 30, height = 15, units = 'cm')
}

ggplot(data = dF_t) +
  geom_line(aes(date, GROS)) +
  facet_wrap(~HruIds, ncol = 6)


