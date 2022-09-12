# GW and SM sensor visualizations

library(fst)
library(dplyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(rstudioapi)


setwd(paste0(dirname(getActiveDocumentContext()$path),"/dHRUM_setup/inputs/Soil_input_data"))
getwd()

GroundWater <- as.data.table(read.fst("vrty.fst")) %>% dcast(ID + date ~ variable, value.var = "value")
Smoisture <- as.data.table(read.fst("vlhkost_od_lukase.fst")) %>% dcast(ID + date ~ variable, value.var = "value")


# BP
################################################
################################################
dta_BP <- as.data.table(readRDS ("D:/project/Setups_For_Dist_Model/inputs/PT_intput_data/BP_D_FG_2021.rds"))
P1_BP <- dta_BP[HruId == 8, ]
names(P1_BP) <- c('date', 'HruId', 'P', 'T')


GW_BP_1 <- GroundWater[ID == '901288001062294',]
SM_BP_11 <- Smoisture[ID =='94206026']
SM_BP_12 <- Smoisture[ID =='94206027']

GwSm_BP1 <- merge(GW_BP_1, SM_BP_11, by ='date') %>% 
  merge(SM_BP_12, by = 'date') %>%
  merge(P1_BP, by = 'date')

Pp <-ggplot(GwSm_BP1) + 
      geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
      scale_y_reverse() + 
      xlab(NULL) + ggtitle("BP I/4")

GWp <- ggplot(GwSm_BP1) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp <- ggplot(GwSm_BP1) +
  geom_line(aes(date, vlhkost.x), color = 'red') + 
  geom_line(aes(date, vlhkost.y)) +
  xlab(NULL)


plot_grid(Pp, SMp, GWp, ncol = 1, align = "v", rel_heights = c(1.5, 3))


################################################

P2_BP <- dta_BP[HruId == 3, ]
names(P2_BP) <- c('date', 'HruId', 'P', 'T')


GW_BP_2 <- GroundWater[ID == '901288001062279',]
SM_BP_21 <- Smoisture[ID =='94206022']
SM_BP_22 <- Smoisture[ID =='94206023']

GwSm_BP2 <- merge(GW_BP_2, SM_BP_21, by ='date') %>% 
  merge(SM_BP_22, by = 'date') %>%
  merge(P2_BP, by = 'date')

Pp_BP2 <-ggplot(GwSm_BP2) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle("BP II/3")

GWp_BP2 <- ggplot(GwSm_BP2) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_BP2 <- ggplot(GwSm_BP2) +
  geom_line(aes(date, vlhkost.x), color = 'red') + 
  geom_line(aes(date, vlhkost.y)) +
  xlab(NULL)


plot_grid(Pp_BP2, SMp_BP2, GWp_BP2, ncol = 1, align = "v", rel_heights = c(1.5, 3))

################################################
P3_BP <- dta_BP[HruId == 3, ]
names(P3_BP) <- c('date', 'HruId', 'P', 'T')

# plot(P3_BP$P, type = 'l')

GW_BP_3 <- GroundWater[ID == '901288001062282',]
SM_BP_31 <- Smoisture[ID =='93148342']
SM_BP_32 <- Smoisture[ID =='93148349']
SM_BP_33 <- Smoisture[ID =='93148351']

GwSm_BP3 <- merge(GW_BP_3, SM_BP_33, by ='date') %>% 
  # merge(SM_BP_33, by = 'date') %>%
  merge(P3_BP, by = 'date')

Pp_BP3 <-ggplot(GwSm_BP3) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle('BP III/2')

GWp_BP3 <- ggplot(GwSm_BP3) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_BP3 <- ggplot(GwSm_BP3) +
  geom_line(aes(date, vlhkost), color = 'red') + 
  # geom_line(aes(date, vlhkost.y)) +
  xlab(NULL)


plot_grid(Pp_BP3, SMp_BP3, GWp_BP3, ncol = 1, align = "v", rel_heights = c(1.5, 3))



###########################################################
GwSm_BP31 <- merge(GW_BP_3, SM_BP_31, by ='date') %>% 
  merge(P3_BP, by = 'date')


Pp_BP31 <-ggplot(GwSm_BP31) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle('BP III/2')

GWp_BP31 <- ggplot(GwSm_BP31) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_BP31 <- ggplot(GwSm_BP31) +
  geom_line(aes(date, vlhkost), color = 'red') + 
  # geom_point(aes(date, vlhkost)) +
  xlab(NULL)

plot_grid(Pp_BP31, SMp_BP31, GWp_BP31, ncol = 1, align = "v", rel_heights = c(1.5, 3))

#-----------------------------------------------------------

GwSm_BP32 <- merge(GW_BP_3, SM_BP_32, by ='date') %>% 
  merge(P3_BP, by = 'date')

sPp_BP32 <-ggplot(GwSm_BP32) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle('BP III/2')

GWp_BP32 <- ggplot(GwSm_BP32) +
  geom_line(aes(date, hladina))
  xlab(NULL)

SMp_BP32 <- ggplot(GwSm_BP32) +
  geom_line(aes(date, vlhkost), color = 'red') + 
  geom_point(aes(date, vlhkost)) +
  xlab(NULL)

plot_grid(Pp_BP32, SMp_BP32, GWp_BP32, ncol = 1, align = "v", rel_heights = c(1.5, 3))


###########################################################
P4_BP <- dta_BP[HruId == 4, ]
names(P4_BP) <- c('date', 'HruId', 'P', 'T')

# plot(P3_BP$P, type = 'l')

GW_BP_4 <- GroundWater[ID == '901288001062280',]
SM_BP_41 <- Smoisture[ID =='93148343']
SM_BP_42 <- Smoisture[ID =='94208512']

GwSm_BP4 <- merge(GW_BP_4, SM_BP_41, by ='date') %>% 
  merge(SM_BP_42, by = 'date') %>%
  merge(P4_BP, by = 'date')

Pp_BP4 <-ggplot(GwSm_BP4) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle('BP II/5')

GWp_BP4 <- ggplot(GwSm_BP4) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_BP4 <- ggplot(GwSm_BP4) +
  geom_line(aes(date, vlhkost.x), color = 'red') + 
  geom_line(aes(date, vlhkost.y)) +
  xlab(NULL)


plot_grid(Pp_BP4, SMp_BP4, GWp_BP4, ncol = 1, align = "v", rel_heights = c(1.5, 3))
###########################################################








# KL
################################################
################################################
dta_KL <- as.data.table(readRDS ("D:/project/Setups_For_Dist_Model/inputs/PT_intput_data/KL_D_FG_2021.rds"))

P1_KL <- dta_KL[HruId == 3, ]
names(P1_KL) <- c('date', 'HruId', 'P', 'T')


GW_KL_1 <- GroundWater[ID == '901288001062286',]
SM_KL_1 <- Smoisture[ID =='94212734']

GwSm_KL1 <- merge(GW_KL_1, SM_KL_1, by ='date') %>% 
  merge(P1_KL, by = 'date')

Pp_KL1 <-ggplot(GwSm_KL1) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle("KL I/7")

GWp_KL <- ggplot(GwSm_KL1) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_KL <- ggplot(GwSm_KL1) +
  geom_line(aes(date, vlhkost), color = 'red') + 
  xlab(NULL)


plot_grid(Pp_KL1, SMp_KL, GWp_KL,  ncol = 1, align = "v", rel_heights = c(1.5, 3))


##########################################
P2_KL <- dta_KL[HruId == 20, ]
names(P2_KL) <- c('date', 'HruId', 'P', 'T')


GW_KL_2 <- GroundWater[ID == '901288001062290',]
SM_KL_21 <- Smoisture[ID =='94212732']
SM_KL_22 <- Smoisture[ID =='94218436']

GwSm_KL2 <- merge(GW_KL_2, SM_KL_21, by ='date') %>% 
  merge(SM_KL_22, by = 'date') %>%
  merge(P2_KL, by = 'date')



Pp_KL2 <-ggplot(GwSm_KL2) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle("KL I/5")

GWp_KL2 <- ggplot(GwSm_KL2) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_KL2 <- ggplot(GwSm_KL2) +
  geom_line(aes(date, vlhkost.x), color = 'red') + 
  geom_line(aes(date, vlhkost.y)) +
  xlab(NULL)


plot_grid(Pp_KL2, SMp_KL2, GWp_KL2,  ncol = 1, align = "v", rel_heights = c(1.5, 3))

##########################################

P3_KL <- dta_KL[HruId == 1, ]
names(P3_KL) <- c('date', 'HruId', 'P', 'T')


GW_KL_3 <- GroundWater[ID == '901288001062277',]
SM_KL_31 <- Smoisture[ID =='94212737']


GwSm_KL3 <- merge(GW_KL_3, SM_KL_31, by ='date') %>% 
  merge(P3_KL, by = 'date')



Pp_KL3 <-ggplot(GwSm_KL3) + 
  geom_bar(aes(x = date, y = P), stat="identity", col = "blue") + 
  scale_y_reverse() + 
  xlab(NULL) + ggtitle("KL II/2")

GWp_KL3 <- ggplot(GwSm_KL3) +
  geom_line(aes(date, hladina)) + 
  xlab(NULL)

SMp_KL3 <- ggplot(GwSm_KL3) +
  geom_line(aes(date, vlhkost), color = 'red') + 
  xlab(NULL)


plot_grid(Pp_KL3, SMp_KL3, GWp_KL3,  ncol = 1, align = "v", rel_heights = c(1.5, 3))

