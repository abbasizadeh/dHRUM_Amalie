source("D:/project/Setups_For_Dist_Model/setups_with_opt/HeatMap/Main.R")

##### EX calibration #####  
# Heatmap

PET_Exp$DTM <- NULL
cor_PET_Exp <- round(cor(PET_Exp),2)

upper_tri <- get_upper_tri(cor_PET_Exp)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Potential Evapotranspiration (Using Expert Knowledge)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  coord_fixed()


#ScatterPlot
ggplot(data = PET_Exp) + geom_point(aes(x = BP_D, y = BP_S), color ="blue") +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge (BP_D vs BP_S)") +
  xlim(0,5)+ylim(0,5)+
  theme(axis.text = element_text(size = 15))+ coord_fixed() 

ggplot(data = PET_Exp) + geom_point(aes(x = KL_D, y = KL_S), color ="blue") +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge (KL_D vs KL_S)") +
  xlim(0,5)+ylim(0,5)+
  theme(axis.text = element_text(size = 15))+ coord_fixed()




####### GW calibration ######
PET_GW$DTM <- NULL
cor_PET_GW <- round(cor(PET_GW),2)

upper_tri <- get_upper_tri(cor_PET_GW)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Potential Evapotranspiration (Using Expert Knowledge and GW Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  coord_fixed()

#ScatterPlot
ggplot(data = PET_GW) + geom_point(aes(x = BP_D, y = BP_S), color ="blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge and GW (BP_D vs BP_S)") +
  xlim(0,8)+ylim(0,8)+
  theme(axis.text = element_text(size = 15))+ coord_fixed()

ggplot(data = PET_GW) + geom_point(aes(x = KL_D, y = KL_S), color ="blue") +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge and GW (KL_D vs KL_S)") +
  xlim(0,25)+ylim(0,25)+
  theme(axis.text = element_text(size = 15))+ coord_fixed()





# SM calibration  
PET_SM$DTM <- NULL
cor_PET_SM <- round(cor(PET_SM),2)

upper_tri <- get_upper_tri(cor_PET_SM)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Potential Evapotranspiration (Using Expert Knowledge and SM Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  coord_fixed()


#ScatterPlot
ggplot(data = PET_SM) + geom_point(aes(x = BP_D, y = BP_S), color ="blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge and SM (BP_D vs BP_S)") +
  xlim(0, 15)+ylim(0, 15)+
  theme(axis.text = element_text(size = 15))+ coord_fixed()

ggplot(data = PET_SM) + geom_point(aes(x = KL_D, y = KL_S), color ="blue") +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge and SM (KL_D vs KL_S)") +
  xlim(0,10)+ylim(0,10)+
  theme(axis.text = element_text(size = 15))+ coord_fixed()




# GW and SM calibration  
PET_GWSM$DTM <- NULL
cor_PET_GWSM <- round(cor(PET_GWSM),2)

upper_tri <- get_upper_tri(cor_PET_GWSM)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Potential Evapotranspiration (Using Expert Knowledge, GW and SM Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  coord_fixed()

#ScatterPlot
ggplot(data = PET_GWSM) + geom_point(aes(x = BP_D, y = BP_S), color ="blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge, GW and SM (BP_D vs BP_S)") +
  xlim(0, 8) + ylim(0, 8) +
  theme(axis.text = element_text(size = 15))+ coord_fixed()

ggplot(data = PET_GWSM) + geom_point(aes(x = KL_D, y = KL_S), color ="blue") +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Potential Evapotranspiration Using Expert Knowledge, GW and SM (KL_D vs KL_S)") +
  xlim(0, 11)+ylim(0, 11)+
  theme(axis.text = element_text(size = 15))+ coord_fixed()



