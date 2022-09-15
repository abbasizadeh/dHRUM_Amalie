source("D:/project/Setups_For_Dist_Model/setups_with_opt/HeatMap/Main.R")

##### EX calibration #####  
# Heatmap

DIRR_Exp$DTM <- NULL
cor_DIRR_Exp <- round(cor(DIRR_Exp),2)

upper_tri <- get_upper_tri(cor_DIRR_Exp)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Direct Runoff (Using Expert Knowledge)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()


#ScatterPlot
ggplot(data = DIRR_Exp) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Direct Runoff (Expert Knowledge)") +
  xlim(0,4)+ylim(0,4)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()

ggplot(data = DIRR_Exp) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Direct Runoff (Expert Knowledge)") +
  xlim(0,0.75)+ylim(0,0.75)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()




####### GW calibration ######
DIRR_GW$DTM <- NULL
cor_DIRR_GW <- round(cor(DIRR_GW),2)

upper_tri <- get_upper_tri(cor_DIRR_GW)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Direct Runoff (Expert Knowledge and GW Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()

#ScatterPlot
ggplot(data = DIRR_GW) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Direct Runoff (Expert Knowledge and GW)") +
  xlim(0,6)+ylim(0,6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()

ggplot(data = DIRR_GW) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Direct Runoff (Expert Knowledge and GW)") +
  xlim(0,6)+ylim(0,6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()





# SM calibration  
DIRR_SM$DTM <- NULL
cor_DIRR_SM <- round(cor(DIRR_SM),2)

upper_tri <- get_upper_tri(cor_DIRR_SM)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Direct Runoff (Expert Knowledge and SM Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()


#ScatterPlot
ggplot(data = DIRR_SM) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Direct Runoff (Expert Knowledge and SM)") +
  xlim(0, 4)+ylim(0, 4)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()

ggplot(data = DIRR_SM) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Direct Runoff (Expert Knowledge and SM)") +
  xlim(0,6)+ylim(0,6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()




# GW and SM calibration  
DIRR_GWSM$DTM <- NULL
cor_DIRR_GWSM <- round(cor(DIRR_GWSM),2)

upper_tri <- get_upper_tri(cor_DIRR_GWSM)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(title = "Direct Runoff (Expert Knowledge, GW and SM Info)") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 20, hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major = element_blank()) + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  coord_fixed()

#ScatterPlot
ggplot(data = DIRR_GWSM) + geom_point(aes(x = BP_D, y = BP_S), color ="blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "BP_D [mm]", y = "BP_S [mm]", title = "Direct Runoff (Expert Knowledge, GW and SM)") +
  xlim(0, 3.5)+ylim(0, 3.5)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()

ggplot(data = DIRR_GWSM) + geom_point(aes(x = KL_D, y = KL_S), color ="blue", size = 2) +
  # xlim(-2, 2) + ylim(-2, 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(x = "KL_D [mm]", y = "KL_S [mm]", title = "Direct Runoff (Expert Knowledge, GW and SM)") +
  xlim(0, 6)+ylim(0, 6)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20))+ coord_fixed()



