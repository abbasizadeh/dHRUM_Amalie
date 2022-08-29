# Assiging Groundwater to each HRU

library(fst)
library(dplyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(RcppDE)
library(terra)
library(sf)
library(sp)


################################################################################
################################################################################
# KL catchment
setwd("D:/project/Setups_For_Dist_Model/inputs/Soil_input_data")
getwd()
GroundWater <- as.data.table(read.fst("vrty.fst")) %>% dcast(ID + date ~ variable, value.var = "value")

# Categorizing GW level based on the distance from the stream

# # level 1 closest to the river
KL_I_4 <- GroundWater[ID == '901288001062125',]
names(KL_I_4) <- c('ID', 'date', 'KL_I_4', 'teplota')
KL_I_4[,c('ID', 'teplota')] <- NULL
# 
KL_I_5 <- GroundWater[ID == '901288001062290',]#[, KL_I_5 := scale(hladina)]
names(KL_I_5) <- c('ID', 'date', 'KL_I_5', 'teplota')
KL_I_5[,c('ID', 'teplota')] <- NULL
# 
KL_II_3 <- GroundWater[ID == '901288001062285',]#[, KL_II_3 := scale(hladina)]
names(KL_II_3) <- c('ID', 'date', 'KL_II_3', 'teplota')
KL_II_3[,c('ID', 'teplota')] <- NULL
# 
KL_II_4 <- GroundWater[ID == '901288001062284',]#[, KL_II_4 := hladina]
names(KL_II_4) <- c('ID', 'date', 'KL_II_4', 'teplota')
KL_II_4[,c('ID', 'teplota')] <- NULL
# 
KL_III_3 <- GroundWater[ID == '901288001062278',]#[, KL_III_3 := scale(hladina)]
names(KL_III_3) <- c('ID', 'date', 'KL_III_3', 'teplota')
KL_III_3[,c('ID', 'teplota')] <- NULL
# 
# 
# ggplot() +
#   geom_line(data = KL_I_4, aes(x = date, y = KL_I_4, color = 'KL_I_4'), size = 0.8)+
#   geom_line(data = KL_I_5, aes(x = date, y = KL_I_5, color = 'KL_I_5')) +
#   geom_line(data = KL_II_3, aes(x = date, y = KL_II_3, color = 'KL_II_3'), size = 0.8) +
#   geom_line(data = KL_II_4, aes(x = date, y = KL_II_4, color = 'KL_II_4'), size = 0.8) +
#   geom_line(data = KL_III_3, aes(x = date, y = KL_III_3, color = 'KL_III_3'), size = 0.8) +
#   ggtitle("level 1")
# 
# KL_GW_l1 <- merge(KL_I_4, KL_I_5, by = 'date')#, all= TRUE)
# KL_GW_l1 <- merge(KL_GW_l1, KL_II_4, by = 'date')#, all= TRUE)
# KL_GW_l1 <- merge(KL_GW_l1, KL_II_3, by = 'date')#, all= TRUE)
# KL_GW_l1 <- merge(KL_GW_l1, KL_III_3, by = 'date')#, all= TRUE)
# 
# KL_GW_l1[, meanGW := (scale(KL_I_4) + scale(KL_I_5) + scale(KL_II_3) + scale(KL_III_3))/4]
# 
# 
# ggplot(data = KL_GW_l1) +
#   geom_line(aes(x = date, y = scale(KL_I_4), color = 'KL_I_4'), size = 0.8)+
#   geom_line(aes(x = date, y = scale(KL_I_5), color = 'KL_I_5')) +
#   # geom_line(aes(x = date, y = scale(KL_II_3), color = 'KL_II_3'), size = 0.8) +
#   geom_line(aes(x = date, y = scale(KL_II_4), color = 'KL_II_4'), size = 0.8) +
#   geom_line( aes(x = date, y = scale(KL_III_3), color = 'KL_III_3'), size = 0.8) +
#   
#   ggtitle("level 1")
# 
# 
# # level 2 
KL_I_6 <- GroundWater[ID == '901288001062289',]#[, KL_I_6 := hladina]
names(KL_I_6) <- c('ID', 'date', 'KL_I_6', 'teplota')
KL_I_6[,c('ID', 'teplota')] <- NULL 

KL_II_2 <- GroundWater[ID == '901288001062277',]#[, KL_II_2 := hladina]
names(KL_II_2) <- c('ID', 'date', 'KL_II_2', 'teplota')
KL_II_2[,c('ID', 'teplota')] <- NULL
# 
# 
# ggplot() +
#   geom_line(data = KL_I_6, aes(x = date, y = KL_I_6, color = 'KL_I_6')) +
#   geom_line(data = KL_II_2, aes(x = date, y = KL_II_2, color = 'KL_II_2'), size = 0.8) +
#   ggtitle("level 2")
# 
# KL_GW_l2 <- merge(KL_I_6, KL_II_2, by = 'date')
# 
#   
# KL_GW_l2[, meanGW:= ((scale(KL_I_6) + scale(KL_II_2)))/2]
# 
# ggplot(data = KL_GW_l2) + 
#   geom_line(aes(x = date, y = scale(KL_I_6)))+
#   geom_line(aes(x = date, y = scale(KL_II_2))) +
#   geom_line(data =KL_GW_l2, aes(x = date, y = meanGW), color = 'red')
# 
# 
# # level 3
KL_I_7 <- GroundWater[ID == '901288001062286',]#[, KL_I_7 := hladina]
names(KL_I_7) <- c('ID', 'date', 'KL_I_7', 'teplota')
KL_I_7[,c('ID', 'teplota')] <- NULL
# 
KL_III_2 <- GroundWater[ID == '901288001062295',]#[, KL_III_2 := hladina]
names(KL_III_2) <- c('ID', 'date', 'KL_III_2', 'teplota')
KL_III_2[,c('ID', 'teplota')] <- NULL
# 
# 
# ggplot() +
#   geom_line(data = KL_I_7, aes(x = date, y = KL_I_7, color = 'KL_I_7'), size = 0.8) +
#   geom_line(data = KL_III_2, aes(x = date, y = KL_III_2, color = 'KL_III_2'), size = 0.8) +
#   ggtitle("level 3")
# 
# 
# KL_GW_l3 <- merge(KL_I_7, KL_III_2, by = 'date')
# KL_GW_l3[, meanGW:= (scale(KL_I_7) + scale(KL_III_2))/2]
# 
# ggplot(data = KL_GW_l3) +
#   geom_line(aes(x = date, y = scale(KL_I_7), color = 'KL_I_7'), size = 0.8) +
#   geom_line(aes(x = date, y = scale(KL_III_2), color = 'KL_III_2'), size = 0.8) +
#   geom_line(aes(x = date, y = meanGW, color = 'meanGW'), size = 0.8) +
#   ggtitle("level 3")
# 
# 
# # level 4
KL_III_1 <- GroundWater[ID == '901288001062291',]#[, KL_III_1 := hladina]
names(KL_III_1) <- c('ID', 'date', 'KL_III_1', 'teplota')
KL_III_1[,c('ID', 'teplota')] <- NULL
# 
# KL_GW_l4 <- merge(KL_III_1, KL_I_7, by = 'date')
# KL_GW_l4[,meanGW := (scale(KL_III_1)+ scale(KL_I_7))/2]
# # KL_GW_l4 <- merge(KL_III_1, KL_I_6, by = 'date')
# # KL_GW_l4 <- merge(KL_GW_l4, KL_II_2, by = 'date')
# # 
# # KL_GW_l4[,meanGW := (scale(KL_III_1)+ scale(KL_I_6) + scale(KL_II_2))/3]
# # 
# ggplot(data = KL_GW_l4, aes(x = date)) +
#   geom_line(aes(x = date, y = scale(KL_III_1), color = 'KL_III_1'), size = 0.8) +
#   geom_line(aes(x = date, y = scale(KL_I_7), color = 'KL_I_7'), size = 0.8) +
#   # geom_line(aes(x = date, y = scale(KL_I_6), color = 'KL_I_6')) +
#   geom_line(aes(x = date, y = meanGW, color = 'meanGW'), size = 0.8) +
#   ggtitle("level 4")

# ggplot() +
#   geom_line(data = KL_III_1, aes(x = date, y = scale(KL_III_1), color = 'KL_III_1'), size = 0.8) +
#   geom_line(data = KL_I_7, aes(x = date, y = scale(KL_I_7), color = 'KL_I_6'), size = 0.8) 
  # geom_line(data = KL_II_2, aes(x = date, y = scale(KL_II_2), color = 'KL_II_2'), size = 0.8)
  

################################################################################
#===============================================================================
# Extracting cell values for each polygon and calculating the weight or contribution of 
# each GW level in each polygon.

setwd("D:/project")
getwd()

Rasterlvl =terra::rast("./Aamlie_Paper/GIS/KL_WT_reclass.tif")
poly = st_read("./Aamlie_Paper/GIS/KL_D_FGWGS.shp") # it is an sf object and terra doesn't like it


poly=vect(poly) # changing sf object into the terra object

plot(Rasterlvl)
plot(poly ,add=TRUE)

# plot(poly)
# poly=st_transform(poly,4326)
# plot(rvk)
# cls=terra::cells(Rasterlvl,poly, touches=TRUE)
# e =terra::extract(Rasterlvl, cls[,'cell']) # raster, cell numbers
dta =terra::extract(Rasterlvl, poly) # raster, cell numbers
# dta =data.table(ID =cls[,1], Rasterlvl= e[,1])
class(dta)
dta <- as.data.table(dta)
# dta$BP_Level1CZ <- as.factor(dta$BP_Level1CZ)

ggplot(dta[ID==4,], aes(factor(KL_WT_reclass))) + 
  geom_bar()

lvl <- data.frame(matrix(ncol = 10, nrow = 23))
colnames(lvl) <- c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8', 'w9', 'w10')


for (i in 1:23) {
  d <- dta[ID == i, ]
  
  n <- length(dta[ID== i, KL_WT_reclass])
  
  lvl[i,1] <- length(d[KL_WT_reclass==1, ID])/n
  lvl[i,2] <- length(d[KL_WT_reclass==2, ID])/n
  lvl[i,3] <- length(d[KL_WT_reclass==3, ID])/n
  lvl[i,4] <- length(d[KL_WT_reclass==4, ID])/n
  lvl[i,5] <- length(d[KL_WT_reclass==5, ID])/n
  lvl[i,6] <- length(d[KL_WT_reclass==6, ID])/n
  lvl[i,7] <- length(d[KL_WT_reclass==7, ID])/n
  lvl[i,8] <- length(d[KL_WT_reclass==8, ID])/n
  lvl[i,9] <- length(d[KL_WT_reclass==9, ID])/n
  lvl[i,10]<-length(d[KL_WT_reclass==10, ID])/n
}


p <- cbind(poly , lvl)
p <- st_as_sf(p)
plot(p)
names(p)
# st_write(p, "D:/project/Aamlie_Paper/GIS/KL_D_FGWGSw.shp")

# Claculating GW level for each HRU
KL_D_Poly <- data.frame(read_sf("D:/project/Aamlie_Paper/GIS/KL_D_FGWGSw.shp"))
KL_D_Weights <- KL_D_Poly[c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8', 'w9', 'w10')]

L1 <- KL_II_3
L2 <- KL_I_4
L3 <- KL_I_5
L4 <- KL_II_4
L5 <- KL_I_7
L6 <- KL_I_6
L7 <- KL_III_3
L8 <- KL_II_2
L9 <- KL_II_2
L10 <- KL_III_1

TS_GW <- merge(L1, L2, by = 'date')
TS_GW <- merge(TS_GW, L3, by = 'date')
TS_GW <- merge(TS_GW, L4, by = 'date')
TS_GW <- merge(TS_GW, L5, by = 'date')
TS_GW <- merge(TS_GW, L6, by = 'date')
TS_GW <- merge(TS_GW, L7, by = 'date')
TS_GW <- merge(TS_GW, L8, by = 'date')
TS_GW <- merge(TS_GW, L9, by = 'date')
TS_GW <- merge(TS_GW, L10, by = 'date')

names(TS_GW) <- c('date', 'L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'L7', 'L8', 'L9', 'L10')
TS_GW$L1 <- scale(TS_GW$L1)
TS_GW$L2 <- scale(TS_GW$L2)
TS_GW$L3 <- scale(TS_GW$L3)
TS_GW$L4 <- scale(TS_GW$L4)
TS_GW$L5 <- scale(TS_GW$L5)
TS_GW$L6 <- scale(TS_GW$L6)
TS_GW$L7 <- scale(TS_GW$L7)
TS_GW$L8 <- scale(TS_GW$L8)
TS_GW$L9 <- scale(TS_GW$L9)
TS_GW$L10 <- scale(TS_GW$L10)


GW_KL_TSs <- list()


for (i in 1:length(KL_D_Poly$OBJECTID)){
  GW_KL_TSs[[i]] <- data.frame(TS_GW$date, (TS_GW$L1 * KL_D_Weights$w1[i]) +
                                 (TS_GW$L2 * KL_D_Weights$w2[i]) + 
                                 (TS_GW$L3 * KL_D_Weights$w3[i]) +
                                 (TS_GW$L4 * KL_D_Weights$w4[i]) +
                                 (TS_GW$L5 * KL_D_Weights$w5[i]) +
                                 (TS_GW$L6 * KL_D_Weights$w6[i]) +
                                 (TS_GW$L7 * KL_D_Weights$w7[i]) +
                                 (TS_GW$L8 * KL_D_Weights$w8[i]) +
                                 (TS_GW$L9 * KL_D_Weights$w9[i]) +
                                 (TS_GW$L10 * KL_D_Weights$w10[i]), i)
  
  names(GW_KL_TSs[[i]]) <- c('date','GW_level', 'HruId')
}


ggplot(data.frame(GW_KL_TSs[[12]]), aes(x = date)) + 
  geom_line(aes(y = GW_level))

# saveRDS(GW_KL_TSs,  file = "D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_KL_D_HRUs.rds")
dta <- readRDS(file ="D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_KL_D_HRUs.rds")


dta <- data.frame(GW_KL_TSs[[1]])
for (i in 2:length(KL_D_Poly$OBJECTID)){
  dta <- rbind(dta ,data.frame(GW_KL_TSs[[i]])) 
}

dta <- as.data.frame(dta)
P <- ggplot(dta, aes(x = date)) + 
  geom_line(aes(y = GW_level))

P + facet_wrap(~HruId, ncol = 6)



#-----------------------------------------------
# linear model
poly = st_read("./Aamlie_Paper/GIS/KL_GW_Points.shp")
GW_point <- as.data.frame(poly)
ggplot(data = GW_point, aes(x =  name, y = WaterTable)) + geom_bar(stat="identity", )


TS_GW<- merge(KL_I_4, KL_I_5, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_I_6, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_I_7, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_II_2, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_II_3, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_II_4, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_III_1, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_III_2, by = 'date', all = TRUE)
TS_GW<- merge(TS_GW, KL_III_3, by = 'date', all = TRUE)

CorTS <- TS_GW[,c(2,3,4,5,6,7,8,9,10,11)]

library(corrr)
GW_KL_MAT <- data.matrix(CorTS)

x <- correlate(GW_KL_MAT)
x %>% filter(KL_I_4 > .7)
x <- GW_KL_MAT %>% correlate() %>% rearrange() %>% shave()

fashion(x)
rplot(x)

correlate(GW_KL_MAT) %>% network_plot(min_cor = 0.7)
corMat

# Loading the shape file of GW sensors
GWSensor_WGS <- st_read('D:/project/Aamlie_Paper/GIS/KL_GW_Points.shp')
sort(GWSensor_WGS$WaterTable)

# Plotting the relationship between elevation and water table
ggplot(data = GWSensor_WGS[c(which(startsWith(GWSensor_WGS$name, 'KL'))),],
        aes(x = WaterTable, y = dem)) + 
  geom_point() + geom_smooth(method='lm') + 
  ggtitle('Water Tale vs Elevation')

lm(dem ~ WaterTable, data = GWSensor_WGS[c(which(startsWith(GWSensor_WGS$name, 'KL'))),])


# Plotting the relationship between distance from the stream and water table
ggplot(data = GWSensor_WGS[c(which(startsWith(GWSensor_WGS$name, 'KL'))),],
       aes(x = WaterTable, y = NEAR_DIST)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('Water tabel vs Distance')


Dmodel <- lm(WaterTable ~ NEAR_DIST, data = GWSensor_WGS[c(which(startsWith(GWSensor_WGS$name, 'KL'))),])
cor(Dmodel$model)
Dmodel


# Plotting the relationship between slope and water table
# Excluding the nearest and farthest poinst from the stream

GWSensor_WGS <- GWSensor_WGS[-c(which(c(GWSensor_WGS$name == 'KL III/1'| GWSensor_WGS$name == 'KL II/3'))),]
GWSensor_WGS[c(which(startsWith(GWSensor_WGS$name, 'KL'))),]

ggplot(data = GWSensor_WGS, aes(x = WaterTable, y = kl_slope_A)) +
   geom_point() +
   geom_smooth(method='lm') +
   ggtitle('Water Table vs Slope')

Lmodel <- lm(WaterTable ~ kl_slope_A, data = GWSensor_WGS)
Lmodel
dta <- as.data.table(GWSensor_WGS[c(which(startsWith(GWSensor_WGS$name, 'KL'))),])
cor(dta$kl_slope_A, dta$WaterTable)


# Mulit varuiable linear regression water table, slope and distance from the outlet
Lmodel <- lm(WaterTable ~ kl_slope_A + NEAR_DIST, data = GWSensor_WGS)

# ----------------- checking the Model
GWSensor_WGS <- st_read('D:/project/Aamlie_Paper/GIS/KL_GW_Points.shp')
WT <- as.data.frame(GWSensor_WGS)
ggplot(data = WT) + 
  geom_point(aes(x = WaterTable, y =kl_wt_M, color = name), size = 5) +
  geom_abline(slope =  1, intercept = 0)


# KL_GW_data <- merge(KL_I_4, KL_I_5, by = 'date')
# # KL_GW_data[,c('ID.x', 'ID.y', 'teplota.x', 'teplota.y', 'ID')] <- NULL
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5')
#
# KL_GW_data <- merge(KL_GW_data, KL_I_6, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6')
#
# KL_GW_data <- merge(KL_GW_data, KL_I_7, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6',
#                        'KL_I_7')
#
# KL_GW_data <- merge(KL_GW_data, KL_II_2, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6',
#                        'KL_I_7', 'KL_II_2')
#
# KL_GW_data <- merge(KL_GW_data, KL_II_3, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6',
#                        'KL_I_7', 'KL_II_2', 'KL_II_3')
#
# KL_GW_data <- merge(KL_GW_data, KL_II_4, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6',
#                        'KL_I_7', 'KL_II_2', 'KL_II_3', 'KL_II_4')
#
# KL_GW_data <- merge(KL_GW_data, KL_III_1, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6',
#                        'KL_I_7', 'KL_II_2', 'KL_II_3', 'KL_II_4', 'KL_III_1')
#
# KL_GW_data <- merge(KL_GW_data, KL_III_2, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6', 'KL_I_7',
#                        'KL_II_2', 'KL_II_3', 'KL_II_4', 'KL_III_1', 'KL_III_2')
# KL_GW_data <- merge(KL_GW_data, KL_III_3, by = 'date')
# names(KL_GW_data) <- c('date', 'KL_I_4', 'KL_I_5', 'KL_I_6', 'KL_I_7',
#                        'KL_II_2', 'KL_II_3', 'KL_II_4', 'KL_III_1', 'KL_III_2', 'KL_III_3')
# my_data <- KL_GW_data[, c(2,3,4,5,6,7,8,9,10,11)]
#
#
# res <- cor(my_data)
#
# library(corrplot)
# corrplot(res, method = c("circle"), type = "upper", order = "hclust",
#          tl.col = "black", tl.srt = 45)

# KL_GWSensor <- st_read('D:/project/Aamlie_Paper/GIS/KL_GW_Points.shp')

# ggplot() +
#   geom_line(data = KL_I_4, aes(x = date, y = hladina, color = 'KL_I_4'))+
#   geom_line(data = KL_I_5, aes(x = date, y = hladina, color = 'KL_I_5'), size = 0.8) +
#   geom_line(data = KL_I_6, aes(x = date, y = hladina, color = 'KL_I_6')) +
#   geom_line(data = KL_I_7, aes(x = date, y = hladina, color = 'KL_I_7'), size = 0.8) +
#   geom_line(data = KL_II_2, aes(x = date, y = hladina, color = 'KL_II_2'), size = 0.8) +
#   geom_line(data = KL_II_3, aes(x = date, y = hladina, color = 'KL_II_3'), size = 0.8) +
#   geom_line(data = KL_II_4, aes(x = date, y = hladina, color = 'KL_II_4'), size = 0.8) +
#   geom_line(data = KL_III_1, aes(x = date, y = hladina, color = 'KL_III_1'), size = 0.8) +
#   geom_line(data = KL_III_2, aes(x = date, y = hladina, color = 'KL_III_2'), size = 0.8) +
#   geom_line(data = KL_III_3, aes(x = date, y = hladina, color = 'KL_III_3'), size = 0.8)



# # ggplot(BP_I_3) + geom_line(aes(date, hldnrm))
# BP_GW_l1 <- merge(BP_III_3, BP_II_5, by = 'date')
# BP_GW_l1 <- merge(BP_GW_l1, BP_I_4, by = 'date')
# # ggplot(data = KL_GW_data, aes(x = date)) +
# #   geom_line(aes(y = KL_I_5)) +
# #   geom_line(aes(y = KL_I_4))
#

#===============================================================================
# Extracting cell values for each polygon and calculating the weight or contribution of 
# each GW level in each polygon.

setwd("D:/project")
getwd()

Rasterlvl =terra::rast("./Aamlie_Paper/GIS/KL_WT_reclass.tif")
poly = st_read("./Aamlie_Paper/GIS/KL_S_FG_WGS.shp") # it is an sf object and terra doesn't like it


poly=vect(poly) # changing sf object into the terra object

plot(Rasterlvl)
plot(poly ,add=TRUE)

# plot(poly)
# poly=st_transform(poly,4326)
# plot(rvk)
# cls=terra::cells(Rasterlvl,poly, touches=TRUE)
# e =terra::extract(Rasterlvl, cls[,'cell']) # raster, cell numbers
dta =terra::extract(Rasterlvl, poly) # raster, cell numbers
# dta =data.table(ID =cls[,1], Rasterlvl= e[,1])
class(dta)
dta <- as.data.table(dta)
# dta$BP_Level1CZ <- as.factor(dta$BP_Level1CZ)

ggplot(dta[ID==4,], aes(factor(KL_WT_reclass))) + 
  geom_bar()

lvl <- data.frame(matrix(ncol = 10, nrow = 23))
colnames(lvl) <- c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8', 'w9', 'w10')


for (i in 1:29) {
  d <- dta[ID == i, ]
  
  n <- length(dta[ID== i, KL_WT_reclass])
  
  lvl[i,1] <- length(d[KL_WT_reclass==1, ID])/n
  lvl[i,2] <- length(d[KL_WT_reclass==2, ID])/n
  lvl[i,3] <- length(d[KL_WT_reclass==3, ID])/n
  lvl[i,4] <- length(d[KL_WT_reclass==4, ID])/n
  lvl[i,5] <- length(d[KL_WT_reclass==5, ID])/n
  lvl[i,6] <- length(d[KL_WT_reclass==6, ID])/n
  lvl[i,7] <- length(d[KL_WT_reclass==7, ID])/n
  lvl[i,8] <- length(d[KL_WT_reclass==8, ID])/n
  lvl[i,9] <- length(d[KL_WT_reclass==9, ID])/n
  lvl[i,10]<-length(d[KL_WT_reclass==10, ID])/n
}


p <- cbind(poly , lvl)
p <- st_as_sf(p)
plot(p)
names(p)
st_write(p, "D:/project/Aamlie_Paper/GIS/KL_S_FGWGSw.shp")

# Claculating GW level for each HRU
KL_S_Poly <- data.frame(read_sf("D:/project/Aamlie_Paper/GIS/KL_S_FGWGSw.shp"))
KL_S_Weights <- KL_S_Poly[c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8', 'w9', 'w10')]

L1 <- KL_II_3
L2 <- KL_I_4
L3 <- KL_I_5
L4 <- KL_II_4
L5 <- KL_I_7
L6 <- KL_I_6
L7 <- KL_III_3
L8 <- KL_II_2
L9 <- KL_II_2
L10 <- KL_III_1

TS_GW <- merge(L1, L2, by = 'date')
TS_GW <- merge(TS_GW, L3, by = 'date')
TS_GW <- merge(TS_GW, L4, by = 'date')
TS_GW <- merge(TS_GW, L5, by = 'date')
TS_GW <- merge(TS_GW, L6, by = 'date')
TS_GW <- merge(TS_GW, L7, by = 'date')
TS_GW <- merge(TS_GW, L8, by = 'date')
TS_GW <- merge(TS_GW, L9, by = 'date')
TS_GW <- merge(TS_GW, L10, by = 'date')

names(TS_GW) <- c('date', 'L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'L7', 'L8', 'L9', 'L10')
TS_GW$L1 <- scale(TS_GW$L1)
TS_GW$L2 <- scale(TS_GW$L2)
TS_GW$L3 <- scale(TS_GW$L3)
TS_GW$L4 <- scale(TS_GW$L4)
TS_GW$L5 <- scale(TS_GW$L5)
TS_GW$L6 <- scale(TS_GW$L6)
TS_GW$L7 <- scale(TS_GW$L7)
TS_GW$L8 <- scale(TS_GW$L8)
TS_GW$L9 <- scale(TS_GW$L9)
TS_GW$L10 <- scale(TS_GW$L10)


GW_KL_TSs <- list()


for (i in 1:length(KL_S_Poly$OBJECTID)){
  GW_KL_TSs[[i]] <- data.frame(TS_GW$date, (TS_GW$L1 * KL_S_Weights$w1[i]) +
                                 (TS_GW$L2 * KL_S_Weights$w2[i]) + 
                                 (TS_GW$L3 * KL_S_Weights$w3[i]) +
                                 (TS_GW$L4 * KL_S_Weights$w4[i]) +
                                 (TS_GW$L5 * KL_S_Weights$w5[i]) +
                                 (TS_GW$L6 * KL_S_Weights$w6[i]) +
                                 (TS_GW$L7 * KL_S_Weights$w7[i]) +
                                 (TS_GW$L8 * KL_S_Weights$w8[i]) +
                                 (TS_GW$L9 * KL_S_Weights$w9[i]) +
                                 (TS_GW$L10 * KL_S_Weights$w10[i]), i)
  
  names(GW_KL_TSs[[i]]) <- c('date','GW_level', 'HruId')
}


ggplot(data.frame(GW_KL_TSs[[12]]), aes(x = date)) + 
  geom_line(aes(y = GW_level))

# saveRDS(GW_KL_TSs,  file = "D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_KL_S_HRUs.rds")
dta <- readRDS(file ="D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_KL_S_HRUs.rds")
