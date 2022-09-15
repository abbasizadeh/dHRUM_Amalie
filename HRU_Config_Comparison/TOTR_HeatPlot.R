source("D:/project/Setups_For_Dist_Model/setups_with_opt/HeatMap/Main.R")

##### EX calibration #####  
# Heatmap

TOTR_Exp$DTM <- NULL
cor_TOTR_Exp <- round(cor(TOTR_Exp),2)

upper_tri <- get_upper_tri(cor_TOTR_Exp)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Total Runoff (Expert Knowledge)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()


#ScatterPlot
ggplot(data = TOTR_Exp) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Total Runoff (Expert Knowledge)") +
  xlim(0,6)+ylim(0,6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 

ggplot(data = TOTR_Exp) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Total Runoff (Expert Knowledge)") +
  xlim(0,5)+ylim(0,5)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 




####### GW calibration ######
TOTR_GW$DTM <- NULL
cor_TOTR_GW <- round(cor(TOTR_GW),2)

upper_tri <- get_upper_tri(cor_TOTR_GW)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Total Runoff (Expert Knowledge and GW Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()

#ScatterPlot
ggplot(data = TOTR_GW) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Total Runoff (Expert Knowledge and GW)") +
  xlim(0,10)+ylim(0,10)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 

ggplot(data = TOTR_GW) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Total Runoff (Expert Knowledge and GW)") +
  xlim(0,6)+ylim(0,6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 





# SM calibration  
TOTR_SM$DTM <- NULL
cor_TOTR_SM <- round(cor(TOTR_SM),2)

upper_tri <- get_upper_tri(cor_TOTR_SM)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Total Runoff (Expert Knowledge and SM Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()


#ScatterPlot
ggplot(data = TOTR_SM) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Total Runoff (Expert Knowledge and SM)") +
  xlim(0, 7)+ylim(0, 7)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 

ggplot(data = TOTR_SM) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Total Runoff (Expert Knowledge and SM)") +
  xlim(0,8)+ylim(0,8)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 




# GW and SM calibration  
TOTR_GWSM$DTM <- NULL
cor_TOTR_GWSM <- round(cor(TOTR_GWSM),2)

upper_tri <- get_upper_tri(cor_TOTR_GWSM)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Total Runoff (Expert Knowledge, GW and SM Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()

#ScatterPlot
ggplot(data = TOTR_GWSM) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Total Runoff (Expert Knowledge, GW and SM)") +
  xlim(0, 6)+ylim(0, 6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 

ggplot(data = TOTR_GWSM) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Total Runoff (Expert Knowledge, GW and SM)") +
  xlim(0, 7)+ylim(0, 7)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed() 



