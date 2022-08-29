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

setwd("D:/project/Setups_For_Dist_Model/inputs/Soil_input_data")
getwd()

GroundWater <- as.data.table(read.fst("vrty.fst")) %>% dcast(ID + date ~ variable, value.var = "value")

# Categorizing GW level based on the distance from the stream


# level 1 closest to the river
BP_III_3 <- GroundWater[ID == '901288001062293',][, hldnrm := scale(hladina)]
BP_II_5 <- GroundWater[ID == '901288001062280',][, hldnrm := scale(hladina)]
BP_I_4 <- GroundWater[ID == '901288001062294',][, hldnrm := scale(hladina)]


# ggplot(BP_I_3) + geom_line(aes(date, hldnrm))
BP_GW_l1 <- merge(BP_III_3, BP_II_5, by = 'date')
BP_GW_l1 <- merge(BP_GW_l1, BP_I_4, by = 'date')

BP_GW_l1[, GWfluc := ((hldnrm.x + hldnrm.y + hldnrm)/3)]

# BP_GW_l1[, .(hldnrm, hldnrm.x, hldnrm.y, GWfluc)=c('BP/III/3', 'BP/II/5', 'BP/II/5', 'Mean') ]

Lgnd1 <- c('BP/III/3', 'BP/II/5', 'BP/II/5', 'Mean')
ggplot(BP_GW_l1, aes(x = date)) + 
  geom_line(aes(y = hldnrm.x, color = 'BP/III/3' )) + 
  geom_line(aes(y = hldnrm.y, color = 'BP/II/5')) + 
  geom_line(aes(y = hldnrm, color = 'BP/I/4')) + 
  geom_line(aes(y = GWfluc, color = 'Mean'), size = 1) +
  labs(x = "Time", y = "GW level", title = "Level 1; BP") + 
  scale_color_manual(values = c("BP/III/3"='blue', 'BP/II/5'='red', 'BP/I/4'='green', 'Mean'='black')) +
  theme(legend.title = element_blank())


# # level 2 second closest to the river
# BP_I_3 <- GroundWater[ID == '901288001062213',][, hldnrm := scale(hladina)]
# BP_GW_l2 <- BP_I_3
# 
# ggplot(BP_GW_l2) + 
#   geom_line(aes(date, hldnrm), size = 1) +
#   labs(x = "Time", y = "GW level", title = "Level 2; BP")


# level 2 second closest to the river
BP_III_2 <- GroundWater[ID == '901288001062282',][, hldnrm := scale(hladina)]
BP_II_4 <- GroundWater[ID == '901288001062292',][, hldnrm := scale(hladina)]

BP_GW_l2 <- merge(BP_III_2, BP_II_4, by = 'date')
BP_GW_l2[, GWfluc := ((hldnrm.x + hldnrm.y)/2)]

ggplot(BP_GW_l2) + 
  geom_line(aes(date, GWfluc), size = 1) + 
  geom_line(aes(date, hldnrm.x, color = 'BP/III/2')) +  
  geom_line(aes(date, hldnrm.y, color = 'BP/II/4')) +
  labs(x = "Time", y = "GW level", title = "Level 2; BP") + 
  scale_color_manual(values = c('BP/III/2'='blue', 'BP/II/4'='red', 'Mean'='black')) +
  theme(legend.title = element_blank())



# level 3 second closest to the river
BP_III_1 <- GroundWater[ID == '901288001062288',][, hldnrm := scale(hladina)]
BP_II_3 <- GroundWater[ID == '901288001062279',][, hldnrm := scale(hladina)]

BP_GW_l3 <- merge(BP_III_1, BP_II_3, by = 'date')
BP_GW_l3[, GWfluc := ((hldnrm.x + hldnrm.y)/2)]

ggplot(BP_GW_l3) + 
  geom_line(aes(date, GWfluc), size = 1) + 
  geom_line(aes(date, hldnrm.x, color = 'BP/III/1')) +  
  geom_line(aes(date, hldnrm.y, color = 'BP/II/3')) +
  labs(x = "Time", y = "GW level", title = "Level 3; BP") + 
  scale_color_manual(values = c('BP/III/1'='blue', 'BP/II/3'='red', 'Mean'='black')) +
  theme(legend.title = element_blank())


# level 4 second closest to the river

BP_II_2 <- GroundWater[ID == '901288001062296',][, hldnrm := scale(hladina)]
BP_II_1 <- GroundWater[ID == '901288001062283',][, hldnrm := scale(hladina)]
BP_GW_l4 <- merge(BP_II_2, BP_II_1, by = 'date')
BP_GW_l4[, GWfluc := ((hldnrm.x + hldnrm.y)/2)]

ggplot(BP_GW_l4) + 
  geom_line(aes(date, GWfluc), size = 1) + 
  geom_line(aes(date, hldnrm.x, color = 'BP/II/2')) +  
  geom_line(aes(date, hldnrm.y, color = 'BP/II/1')) +
  labs(x = "Time", y = "GW level", title = "Level 4; BP")+ 
  scale_color_manual(values = c('BP/II/2'='blue', 'BP/II/1'='red', 'Mean'='black')) +
  theme(legend.title = element_blank())

  
#===============================================================================
# Extracting cell values for each polygon and calculating the weight or contribution of 
# each GW level in each polygon.

setwd("D:/project")
getwd()

Rasterlvl =terra::rast("./Aamlie_Paper/GIS/BP_Level1CZ.tif")
poly = st_read("./Aamlie_Paper/GIS/BP_D_FG_WGS.shp") # it is an sf object and terra doesn't like it


poly=vect(poly) # changing sf object into the terra object

plot(Rasterlvl)
plot(p ,add=TRUE)

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

ggplot(dta[ID==4,], aes(BP_Level1CZ)) + 
  geom_bar()

lvl <- data.frame(matrix(ncol = 4, nrow = 39))
colnames(lvl) <- c('w1', 'w2', 'w3', 'w4')


for (i in 1:39) {
  d <- dta[ID==i,]
  n <- length(dta[ID== i, BP_Level1CZ])
  lvl[i,1] <- length(d[BP_Level1CZ==1, ID])/n
  lvl[i,2] <- length(d[BP_Level1CZ==2, ID])/n
  lvl[i,3] <- length(d[BP_Level1CZ==3, ID])/n
  lvl[i,4] <- length(d[BP_Level1CZ==4 | BP_Level1CZ==5, ID])/n
}


p <- cbind(poly , lvl)
p <- st_as_sf(p)
plot(p)
names(p)
# st_write(p, "D:/project/Aamlie_Paper/GIS/BP_D_WGS.shp")


plory <- rbind(plory , lvl)
meanLevel <- 
  dta %>% 
  group_by(ID) %>% 
  rbind(dta, lvl)

(
KS_county_sf <- 
  #--- back to sf ---#
  st_as_sf(poly) %>% 
  #--- define ID ---#
  mutate(ID := seq_len(nrow(.))) %>% 
  #--- merge by ID ---#
  left_join(., meanLevel, by = "ID")
)
plot(KS_county_sf)


################################################################################
################################################################################
# Calculating GW level time series for each HRU in BP_D

BP_Poly <- data.frame(read_sf("D:/project/Aamlie_Paper/GIS/BP_D_WGS.shp"))
BP_D_Weights <- BP_Poly[c('w1', 'w2', 'w3', 'w4')]

class(BP_GW_l1)
BP_GW_l1[,c("ID.x","hladina.x", "teplota.x", "hldnrm.x",  "ID.y","hladina.y", "teplota.y", "hldnrm.y","ID","hladina","teplota","hldnrm")] <- NULL
names(BP_GW_l1) <- c('date', 'L1')

BP_GW_l2[,c("ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y")] <- NULL
names(BP_GW_l2) <- c('date', 'L2')

BP_GW_l3[,c("ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y" )] <- NULL
names(BP_GW_l3) <- c('date', 'L3')

BP_GW_l4[,c( "ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y" )] <- NULL
names(BP_GW_l4) <- c('date', 'L4')

GW_BP_dtFrame <- merge(BP_GW_l1, BP_GW_l2, by = 'date')
GW_BP_dtFrame <- merge(GW_BP_dtFrame, BP_GW_l3, by = 'date')
GW_BP_dtFrame <- merge(GW_BP_dtFrame, BP_GW_l4, by = 'date')

GW_BP_TSs <- list()

for (i in 1:39){
  GW_BP_TSs[[i]] <- data.frame(GW_BP_dtFrame$date, (GW_BP_dtFrame$L1 * BP_D_Weights$w1[i]) +
    (GW_BP_dtFrame$L2 * BP_D_Weights$w2[i]) + 
    (GW_BP_dtFrame$L3 * BP_D_Weights$w3[i]) +
    (GW_BP_dtFrame$L4 * BP_D_Weights$w4[i]), i)
  
  names(GW_BP_TSs[[i]]) <- c('date','GW_level', 'HruId')
  
  }

"D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater"


ggplot(data.frame(GW_BP_TSs[[1]]), aes(x = date)) + 
  geom_line(aes(y = GW_level))

saveRDS(GW_BP_TSs,  file = "D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_HRUs.rds")
# dta <- readRDS(file ="D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_HRUs.rds")
dta <- data.frame(GW_BP_TSs[[1]])
for (i in 2:39){
  dta <- rbind(dta ,data.frame(GW_BP_TSs[[i]])) 
}

P <- ggplot(dta, aes(x = date)) + 
  geom_line(aes(y = GW_level))

P + facet_wrap(~HruId, ncol = 6)


################################################################################
################################################################################
# Producing GW level time series for each HRU in BP_S

Rasterlvl = terra::rast("D:/project/Aamlie_Paper/GIS/BP_LevelWGS.tif")
poly = st_read("D:/project/Aamlie_Paper/GIS/BP_S_WGS.shp") # it is an sf object and terra doesn't like it


poly = vect(poly) # changing sf object into the terra object

plot(Rasterlvl)
plot(poly ,add=TRUE)

# plot(poly)
# poly=st_transform(poly,4326)
# plot(rvk)
# cls=terra::cells(Rasterlvl,poly, touches=TRUE)
# e =terra::extract(Rasterlvl, cls[,'cell']) # raster, cell numbers
dta = terra::extract(Rasterlvl, poly) # raster, cell numbers
# dta =data.table(ID =cls[,1], Rasterlvl= e[,1])
class(dta)
dta <- as.data.table(dta)
# dta$BP_Level1CZ <- as.factor(dta$BP_Level1CZ)

ggplot(dta[ID==4,], aes(BP_LevelWGS)) + 
  geom_bar()

lvl <- data.frame(matrix(ncol = 4, nrow = length(poly$OBJECTID)))
colnames(lvl) <- c('w1', 'w2', 'w3', 'w4')


for (i in 1:length(poly$OBJECTID)) {
  d <- dta[ID==i,]
  n <- length(dta[ID== i, BP_LevelWGS])
  lvl[i,1] <- length(d[BP_LevelWGS==1, ID])/n
  lvl[i,2] <- length(d[BP_LevelWGS==2, ID])/n
  lvl[i,3] <- length(d[BP_LevelWGS==3, ID])/n
  lvl[i,4] <- length(d[BP_LevelWGS==4 | BP_LevelWGS==5, ID])/n
}


p <- cbind(poly , lvl)
p <- st_as_sf(p)
plot(p)
names(p)
st_write(p, "D:/project/Aamlie_Paper/GIS/BP_S_FG_WGS.shp")



BP_Poly <- data.frame(read_sf("D:/project/Aamlie_Paper/GIS/BP_S_FG_WGS.shp"))
BP_D_Weights <- BP_Poly[c('w1', 'w2', 'w3', 'w4')]

class(BP_GW_l1)
BP_GW_l1[,c("ID.x","hladina.x", "teplota.x", "hldnrm.x",  "ID.y","hladina.y", "teplota.y", "hldnrm.y","ID","hladina","teplota","hldnrm")] <- NULL
names(BP_GW_l1) <- c('date', 'L1')

BP_GW_l2[,c("ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y")] <- NULL
names(BP_GW_l2) <- c('date', 'L2')

BP_GW_l3[,c("ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y" )] <- NULL
names(BP_GW_l3) <- c('date', 'L3')

BP_GW_l4[,c( "ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y" )] <- NULL
names(BP_GW_l4) <- c('date', 'L4')


GW_BP_dtFrame <- merge(BP_GW_l1, BP_GW_l2, by = 'date')
GW_BP_dtFrame <- merge(GW_BP_dtFrame, BP_GW_l3, by = 'date')
GW_BP_dtFrame <- merge(GW_BP_dtFrame, BP_GW_l4, by = 'date')


GW_BP_TSs <- list()


for (i in 1:length(poly$OBJECTID)){
  GW_BP_TSs[[i]] <- data.frame(GW_BP_dtFrame$date, (GW_BP_dtFrame$L1 * BP_D_Weights$w1[i]) +
                                 (GW_BP_dtFrame$L2 * BP_D_Weights$w2[i]) + 
                                 (GW_BP_dtFrame$L3 * BP_D_Weights$w3[i]) +
                                 (GW_BP_dtFrame$L4 * BP_D_Weights$w4[i]), i)
  
  names(GW_BP_TSs[[i]]) <- c('date','GW_level', 'HruId')
}


ggplot(data.frame(GW_BP_TSs[[1]]), aes(x = date)) + 
  geom_line(aes(y = GW_level))

saveRDS(GW_BP_TSs,  file = "D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_BP_S_HRUs.rds")
dta <- readRDS(file ="D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_BP_S_HRUs.rds")
dta <- data.frame(GW_BP_TSs[[1]])
for (i in 2:length(poly$OBJECTID)){
  dta <- rbind(dta ,data.frame(GW_BP_TSs[[i]])) 
}

P <- ggplot(dta, aes(x = date)) + 
  geom_line(aes(y = GW_level))

P + facet_wrap(~HruId, ncol = 6)



 
# BP_GW_l1[, .(hldnrm, hldnrm.x, hldnrm.y, GWfluc)=c('BP/III/3', 'BP/II/5', 'BP/II/5', 'Mean') ]

Lgnd1 <- c('BP/III/3', 'BP/II/5', 'BP/II/5', 'Mean')
ggplot(BP_GW_l1, aes(x = date)) +
  geom_line(aes(y = hldnrm.x, color = 'BP/III/3' )) +
  geom_line(aes(y = hldnrm.y, color = 'BP/II/5')) +
  geom_line(aes(y = hldnrm, color = 'BP/I/4')) +
  geom_line(aes(y = GWfluc, color = 'Mean'), size = 1) +
  labs(x = "Time", y = "GW level", title = "Level 1; BP") +
  scale_color_manual(values = c("BP/III/3"='blue', 'BP/II/5'='red', 'BP/I/4'='green', 'Mean'='black')) +
  theme(legend.title = element_blank())


# # level 2 second closest to the river
# BP_I_3 <- GroundWater[ID == '901288001062213',][, hldnrm := scale(hladina)]
# BP_GW_l2 <- BP_I_3
# 
# ggplot(BP_GW_l2) + 
#   geom_line(aes(date, hldnrm), size = 1) +
#   labs(x = "Time", y = "GW level", title = "Level 2; BP")


# # level 2 second closest to the river
# BP_III_2 <- GroundWater[ID == '901288001062282',][, hldnrm := scale(hladina)]
# BP_II_4 <- GroundWater[ID == '901288001062292',][, hldnrm := scale(hladina)]
# 
# BP_GW_l2 <- merge(BP_III_2, BP_II_4, by = 'date')
# BP_GW_l2[, GWfluc := ((hldnrm.x + hldnrm.y)/2)]
# 
# ggplot(BP_GW_l2) + 
#   geom_line(aes(date, GWfluc), size = 1) + 
#   geom_line(aes(date, hldnrm.x, color = 'BP/III/2')) +  
#   geom_line(aes(date, hldnrm.y, color = 'BP/II/4')) +
#   labs(x = "Time", y = "GW level", title = "Level 2; BP") + 
#   scale_color_manual(values = c('BP/III/2'='blue', 'BP/II/4'='red', 'Mean'='black')) +
#   theme(legend.title = element_blank())
# 
# 
# 
# # level 3 second closest to the river
# BP_III_1 <- GroundWater[ID == '901288001062288',][, hldnrm := scale(hladina)]
# BP_II_3 <- GroundWater[ID == '901288001062279',][, hldnrm := scale(hladina)]
# 
# BP_GW_l3 <- merge(BP_III_1, BP_II_3, by = 'date')
# BP_GW_l3[, GWfluc := ((hldnrm.x + hldnrm.y)/2)]
# 
# ggplot(BP_GW_l3) + 
#   geom_line(aes(date, GWfluc), size = 1) + 
#   geom_line(aes(date, hldnrm.x, color = 'BP/III/1')) +  
#   geom_line(aes(date, hldnrm.y, color = 'BP/II/3')) +
#   labs(x = "Time", y = "GW level", title = "Level 3; BP") + 
#   scale_color_manual(values = c('BP/III/1'='blue', 'BP/II/3'='red', 'Mean'='black')) +
#   theme(legend.title = element_blank())
# 
# 
# # level 4 second closest to the river
# 
# BP_II_2 <- GroundWater[ID == '901288001062296',][, hldnrm := scale(hladina)]
# BP_II_1 <- GroundWater[ID == '901288001062283',][, hldnrm := scale(hladina)]
# BP_GW_l4 <- merge(BP_II_2, BP_II_1, by = 'date')
# BP_GW_l4[, GWfluc := ((hldnrm.x + hldnrm.y)/2)]
# 
# ggplot(BP_GW_l4) + 
#   geom_line(aes(date, GWfluc), size = 1) + 
#   geom_line(aes(date, hldnrm.x, color = 'BP/II/2')) +  
#   geom_line(aes(date, hldnrm.y, color = 'BP/II/1')) +
#   labs(x = "Time", y = "GW level", title = "Level 4; BP")+ 
#   scale_color_manual(values = c('BP/II/2'='blue', 'BP/II/1'='red', 'Mean'='black')) +
#   theme(legend.title = element_blank())
# 
# 
# 
# 
# 
# # Producing GW level time series for each HRU in KL_D
# 
# Rasterlvl =terra::rast("D:/project/Aamlie_Paper/GIS/KL_LevelWGS.tif")
# poly = st_read("D:/project/Aamlie_Paper/GIS/BP_S_WGS.shp") # it is an sf object and terra doesn't like it
# 
# 
# poly=vect(poly) # changing sf object into the terra object
# 
# plot(Rasterlvl)
# plot(poly ,add=TRUE)
# 
# # plot(poly)
# # poly=st_transform(poly,4326)
# # plot(rvk)
# # cls=terra::cells(Rasterlvl,poly, touches=TRUE)
# # e =terra::extract(Rasterlvl, cls[,'cell']) # raster, cell numbers
# dta =terra::extract(Rasterlvl, poly) # raster, cell numbers
# # dta =data.table(ID =cls[,1], Rasterlvl= e[,1])
# class(dta)
# dta <- as.data.table(dta)
# # dta$BP_Level1CZ <- as.factor(dta$BP_Level1CZ)
# 
# ggplot(dta[ID==4,], aes(BP_LevelWGS)) + 
#   geom_bar()
# 
# lvl <- data.frame(matrix(ncol = 4, nrow = length(poly$OBJECTID)))
# colnames(lvl) <- c('w1', 'w2', 'w3', 'w4')
# 
# 
# for (i in 1:length(poly$OBJECTID)) {
#   d <- dta[ID==i,]
#   n <- length(dta[ID== i, BP_LevelWGS])
#   lvl[i,1] <- length(d[BP_LevelWGS==1, ID])/n
#   lvl[i,2] <- length(d[BP_LevelWGS==2, ID])/n
#   lvl[i,3] <- length(d[BP_LevelWGS==3, ID])/n
#   lvl[i,4] <- length(d[BP_LevelWGS==4 | BP_LevelWGS==5, ID])/n
# }
# 
# 
# p <- cbind(poly , lvl)
# p <- st_as_sf(p)
# plot(p)
# names(p)
# st_write(p, "D:/project/Aamlie_Paper/GIS/BP_S_FG_WGS.shp")
# 
# 
# 
# BP_Poly <- data.frame(read_sf("D:/project/Aamlie_Paper/GIS/BP_S_FG_WGS.shp"))
# BP_D_Weights <- BP_Poly[c('w1', 'w2', 'w3', 'w4')]
# 
# class(BP_GW_l1)
# BP_GW_l1[,c("ID.x","hladina.x", "teplota.x", "hldnrm.x",  "ID.y","hladina.y", "teplota.y", "hldnrm.y","ID","hladina","teplota","hldnrm")] <- NULL
# names(BP_GW_l1) <- c('date', 'L1')
# 
# BP_GW_l2[,c("ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y")] <- NULL
# names(BP_GW_l2) <- c('date', 'L2')
# 
# BP_GW_l3[,c("ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y" )] <- NULL
# names(BP_GW_l3) <- c('date', 'L3')
# 
# BP_GW_l4[,c( "ID.x", "hladina.x", "teplota.x", "hldnrm.x", "ID.y", "hladina.y", "teplota.y", "hldnrm.y" )] <- NULL
# names(BP_GW_l4) <- c('date', 'L4')
# 
# 
# GW_BP_dtFrame <- merge(BP_GW_l1, BP_GW_l2, by = 'date')
# GW_BP_dtFrame <- merge(GW_BP_dtFrame, BP_GW_l3, by = 'date')
# GW_BP_dtFrame <- merge(GW_BP_dtFrame, BP_GW_l4, by = 'date')
# 
# 
# GW_BP_TSs <- list()
# 
# 
# for (i in 1:length(poly$OBJECTID)){
#   GW_BP_TSs[[i]] <- data.frame(GW_BP_dtFrame$date, (GW_BP_dtFrame$L1 * BP_D_Weights$w1[i]) +
#                                  (GW_BP_dtFrame$L2 * BP_D_Weights$w2[i]) + 
#                                  (GW_BP_dtFrame$L3 * BP_D_Weights$w3[i]) +
#                                  (GW_BP_dtFrame$L4 * BP_D_Weights$w4[i]), i)
#   
#   names(GW_BP_TSs[[i]]) <- c('date','GW_level', 'HruId')
# }
# 
# "D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater"
# 
# 
# ggplot(data.frame(GW_BP_TSs[[1]]), aes(x = date)) + 
#   geom_line(aes(y = GW_level))
# 
# saveRDS(GW_BP_TSs,  file = "D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_BP_S_HRUs.rds")
# dta <- readRDS(file ="D:/project/Setups_For_Dist_Model/inputs/Soil_input_data/SoilMoist_Groundwater/GW_BP_S_HRUs.rds")
# dta <- data.frame(GW_BP_TSs[[1]])
# for (i in 2:length(poly$OBJECTID)){
#   dta <- rbind(dta ,data.frame(GW_BP_TSs[[i]])) 
# }
# 
# P <- ggplot(dta, aes(x = date)) + 
#   geom_line(aes(y = GW_level))
# 
# P + facet_wrap(~HruId, ncol = 6)
# 