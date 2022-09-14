# Catchment characteristics. 
library(terra)
library(sf)
library(data.table)

setwd("D:/project/Amalie_Paper")


# SlopeR =terra::rast("D:/project/Setups_For_Dist_Model/setups_with_opt/slope/slopeWGS.tif")
SlopeR =terra::rast("./GIS/dem1.tif")
poly = st_read("./GIS/BP_D_WGS.shp")
# poly = st_read("D:/project/Aamlie_Paper/GIS/KL_D_FG_CZ.shp")
# poly = st_read("D:/project/Aamlie_Paper/GIS/BP.shp")

# plot(poly)
# poly=st_transform(poly,4326)
# poly=terra::vect(as(poly,'Spatial'))
poly=terra::vect(poly)
# plot(SlopeR)
# plot(poly,add=TRUE)
cls=terra::cells(SlopeR,poly, touches=TRUE)
e =terra::extract(SlopeR, cls[,'cell']) # raster, cell numbers
dta =data.table(ID =cls[,1],SlopeR= e[,1])


meanSlope <- mean(dta$SlopeR)
meanSlope


# Annual rainfall and tempreture
dtaDF_BP <- as.data.table(readRDS ("./Rscripts/dHRUM_setup/inputs/PT_intput_data/BP_D_FG_2021.rds"))
dtaDF_BP[, Y:=format(DTM, format="%Y")]
PrecBP <- dtaDF_BP[, sum(P), by = Y]
plot(PrecBP$V1, type = "l")
PrecBP[, mean(PrecBP$V1)]


dtaDF_KL <- as.data.table(readRDS ("./Rscripts/dHRUM_setup/inputs/PT_intput_data/KL_D_FG_2021.rds"))
dtaDF_KL[, Y:=format(DTM, format="%Y")]
PrecKL <- dtaDF_BP[, sum(P), by = .(Y)]
plot(PrecKL$V1, type = "l")
PrecBP[, mean(PrecKL$V1)]



dhrumInput <- read.delim2("D:/project/dHRUM/dHRUM/Calibrations/Amalie/indata/BP_1960_01_01.txt",
                          header = TRUE, sep = " ")

dhrumInput$X01.1 <- NULL
names(dhrumInput) = c("P", "T")
