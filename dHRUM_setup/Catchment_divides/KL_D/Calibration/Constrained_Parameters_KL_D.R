library(data.table)


# Parameters (For Building)
dtHrus <- as.data.table(read.csv("./inputs/Soil_input_data/Forest_Geo/KL_Drainage_FG.csv"))
SoilKL <- dtHrus #[Povodi=='KL',]
NhrusKL <- nrow(SoilKL)

nHrus <- NhrusKL
ParDFup1 = data.frame(B_SOIL = 3., C_MAX = 1000, B_EVAP = 2,  KS = 0.02, KF = 0.99, ADIV = 1, CDIV = 0.1,
                      SDIV = 0.1, CAN_ST = 1, STEM_ST = 0.5, CSDIV = 0.5, TETR = 5, DDFA = 2, TMEL = -0.1,
                      RETCAP = 20, CMIN=400)

parsvec=as.numeric(ParDFup1[1,])
for(i in 1:(nHrus-1)){
  ParDFup1 <- rbind(ParDFup1,parsvec)
}


ParDFlow1 = data.frame( B_SOIL = 0, C_MAX = 401, B_EVAP = 0.6,  KS = 0.01, KF = 0, ADIV = 0.8, CDIV = 0,
                        SDIV = 0, CAN_ST = 0.01, STEM_ST = 0.001, CSDIV = 0, TETR = 1, DDFA = 0, TMEL = -2.0,
                        RETCAP = 5, CMIN=300)

parsvec=as.numeric(ParDFlow1[1,])
for(i in 1:(nHrus-1)){
  ParDFlow1 <- rbind(ParDFlow1,parsvec)
}


# Parameter constraints

# Constraining ADIV using Normalized Slope,
SlopeDist <- readRDS(file ="./outputs/SlopeDist_KL_D_FG.rds")
# write.csv(SlopeDist,"./setups_with_opt/KL_D_FG/Slope.csv", row.names = F)


indexF <- which(SoilKL$Land_Use=="Forest")
indexA <- which(SoilKL$Land_Use=="Arable Land" | SoilKL$Land_Use=="Meadow")
indexW <- which(SoilKL$Land_Use=="Wetland")

# Soil Storage
# C_Max, CMIN and B_Soil
CCB <- readRDS(file ="./outputs/CCB_KL_D_FG.rds")

# difference between C_max and C_min
C_diff <- (CCB$Cmax-CCB$Cmin)

ParDFup1$C_MAX <- CCB$Cmax + (CCB$Cmax * 0.8)
ParDFlow1$C_MAX <- CCB$Cmax-(C_diff*0.05)

ParDFup1$CMIN <-  CCB$Cmin+(C_diff*0.1)
ParDFlow1$CMIN <- CCB$Cmin -(CCB$Cmin*0.8)

ParDFup1$B_SOIL <- CCB$b + (abs(CCB$b) * 0.1)
ParDFlow1$B_SOIL <- CCB$b - (abs(CCB$b)*0.1)


ParDFup1$C_MAX[indexW] <- CCB$Cmax[indexW] + (CCB$Cmax[indexW]*0.8) # wetland
ParDFlow1$C_MAX[indexW] <- CCB$Cmax[indexW]# - (C_diff*0.05)

ParDFup1$CMIN[indexW] <-  CCB$Cmin[indexW] - (C_diff[indexW]*0.2)
ParDFlow1$CMIN[indexW] <- CCB$Cmin[indexW] - (CCB$Cmin[indexW]*0.8)

# ADIV constraints
ParDFup1$ADIV[indexF] <- SlopeDist$Slope[indexF] - (SlopeDist$Slope[indexF] * 0.2)  #ADIV up Forest
ParDFlow1$ADIV[indexF] <- SlopeDist$Slope[indexF] - (SlopeDist$Slope[indexF] * 0.8)  #ADIV low Forest

ParDFup1$ADIV[indexA] <- SlopeDist$Slope[indexA] + (SlopeDist$Slope[indexA] * 0.5) #ADIV up Arable land
ParDFlow1$ADIV[indexA] <- SlopeDist$Slope[indexA] - (SlopeDist$Slope[indexA] * 0.2) #ADIV up Arable land

ParDFup1$ADIV[indexW] <- SlopeDist$Slope[indexW] - (SlopeDist$Slope[indexW] * 0.2)  #ADIV up Forest
ParDFlow1$ADIV[indexW] <- SlopeDist$Slope[indexW] - (SlopeDist$Slope[indexW] * 0.8)  #ADIV up Forest
# for (i in 1:length(ParDFup1$ADIV)){
#   if(ParDFup1$ADIV[i] >= 1){ParDFup1$ADIV[i] = 0.99}
# }

# KS using mean(Norm Slope and Norm Distance)
# ParDFup1$KS[indexF] <- SlopeDist$meanDS[indexF] - (SlopeDist$meanDS[indexF] * 0.1) #KS up Forest
# ParDFlow1$KS[indexF] <- SlopeDist$meanDS[indexF]- (SlopeDist$meanDS[indexF] * 0.7) #KS low Forest
# 
# ParDFup1$KS[indexA] <- SlopeDist$meanDS[indexA] + (SlopeDist$meanDS[indexA] * 0.3) #KS up Arable land
# ParDFlow1$KS[indexA] <- SlopeDist$meanDS[indexA] - (SlopeDist$meanDS[indexA] * 0.3)  #KS low Arable land
# 
# ParDFup1$KS[indexW] <- SlopeDist$meanDS[indexW] - (SlopeDist$meanDS[indexW] * 0.0) #KS up Wetland
# ParDFlow1$KS[indexW] <- SlopeDist$meanDS[indexW] - (SlopeDist$meanDS[indexW] * 0.8)  #KS low Wetland

# CDIV and SDIV;  0 < CDIV+SDIV < 1
ParDFup1$CDIV[indexF] <- 0.7
ParDFlow1$CDIV[indexF] <- 0.3
# ParDFup1$CDIV[indexF] <- 0.9
# ParDFlow1$CDIV[indexF] <- 0.3

ParDFup1$CDIV[indexA] <- 0.5
ParDFlow1$CDIV[indexA] <- 0.01
# ParDFup1$CDIV[indexA] <- 0.4
# ParDFlow1$CDIV[indexA] <- 0.01

# ParDFup1$CDIV[indexW] <- 0.5
# ParDFlow1$CDIV[indexW] <- 0.1
ParDFup1$CDIV[indexW] <- 0.4
ParDFlow1$CDIV[indexW] <- 0.1

# SDIV
# ParDFup1$SDIV[indexF] <- 0.4
# ParDFlow1$SDIV[indexF] <- 0.01
ParDFup1$SDIV[indexF] <- 0.3
ParDFlow1$SDIV[indexF] <- 0.01

ParDFup1$SDIV[indexA] <- 0.3
ParDFlow1$SDIV[indexA] <- 0.01
# ParDFup1$SDIV[indexA] <- 0.2
# ParDFlow1$SDIV[indexA] <- 0.01

# ParDFup1$SDIV[indexW] <- 0.5
# ParDFlow1$SDIV[indexW] <- 0.01
ParDFup1$SDIV[indexW] <- 0.4
ParDFlow1$SDIV[indexW] <- 0.01

# Constraining RETCAP
SDC = readRDS(file = "./outputs/SDC_KL_D_FG.rds")
ParDFup1$RETCAP[indexF] <- SDC$SDC_2[indexF]*10 - ((SDC$SDC_2[indexF]*10) * 0.20)
ParDFlow1$RETCAP[indexF] <- SDC$SDC_2[indexF]*10 - ((SDC$SDC_2[indexF]*10) * 0.40)

ParDFup1$RETCAP[indexA] <- SDC$SDC_2[indexA]*10 + ((SDC$SDC_2[indexA]*10) * 0.80)
ParDFlow1$RETCAP[indexA] <- SDC$SDC_2[indexA]*10 - ((SDC$SDC_2[indexA]*10) * 0.50)

ParDFup1$RETCAP[indexW] <- SDC$SDC_2[indexW]*10 - ((SDC$SDC_2[indexW]*10) * 0.20)
ParDFlow1$RETCAP[indexW] <- SDC$SDC_2[indexW]*10 - ((SDC$SDC_2[indexW]*10) * 0.40)

# Constraining Forest and Arable land canopy storage STEM_ST = 0.1
# CAN_ST
ParDFup1$CAN_ST[indexF] <- 3       # Forest Up         STEM_ST = 1.5
ParDFlow1$CAN_ST[indexF] <- 1      # Forest Low

ParDFup1$CAN_ST[indexA] <- 2     # Arable Land Up
ParDFlow1$CAN_ST[indexA] <- 0.01    # Arable Land Low
# ParDFup1$CAN_ST[indexA] <- 1.5     # Arable Land Up
# ParDFlow1$CAN_ST[indexA] <- 0.5    # Arable Land Low

# ParDFup1$CAN_ST[indexW] <- 4     # Arable Wetland
# ParDFlow1$CAN_ST[indexW] <- 0.01    # Arable Wetland
ParDFup1$CAN_ST[indexW] <- 1.3     # Arable Wetland
ParDFlow1$CAN_ST[indexW] <- 0.5    # Arable Wetland

# STEM_ST
# ParDFup1$STEM_ST[indexF] <- 6
# ParDFlow1$STEM_ST[indexF] <- 0.05
ParDFup1$STEM_ST[indexF] <- 2
ParDFlow1$STEM_ST[indexF] <- 1

ParDFup1$STEM_ST[indexA] <- 2
ParDFlow1$STEM_ST[indexA] <- 0.01
# ParDFup1$STEM_ST[indexA] <- 1.3
# ParDFlow1$STEM_ST[indexA] <- 0.1

# ParDFup1$STEM_ST[indexW] <- 2
# ParDFlow1$STEM_ST[indexW] <- 0.01
ParDFup1$STEM_ST[indexW] <- 1.1
ParDFlow1$STEM_ST[indexW] <- 0.1

# B_Evap
# ParDFup1$B_EVAP[indexF] <- 6
# ParDFlow1$B_EVAP[indexF] <- 0.01
ParDFup1$B_EVAP[indexF] <- 5
ParDFlow1$B_EVAP[indexF] <- 0.01

ParDFup1$B_EVAP[indexA] <- 10
ParDFlow1$B_EVAP[indexA] <- 0.01
# ParDFup1$B_EVAP[indexA] <- 8
# ParDFlow1$B_EVAP[indexA] <- 0.01

# ParDFup1$B_EVAP[indexW] <- 8
# ParDFlow1$B_EVAP[indexW] <- 0.01
ParDFup1$B_EVAP[indexW] <- 6
ParDFlow1$B_EVAP[indexW] <- 0.01

# TMEL
# ParDFup1$TMEL[indexF] <- 0
# ParDFlow1$TMEL[indexF] <- -20
ParDFup1$TMEL[indexF] <- -5
ParDFlow1$TMEL[indexF] <- -20

ParDFup1$TMEL[indexA] <- 0
ParDFlow1$TMEL[indexA] <- -10

# ParDFup1$TMEL[indexW] <- 0
# ParDFlow1$TMEL[indexW] <- -8
ParDFup1$TMEL[indexW] <- -2
ParDFlow1$TMEL[indexW] <- -8
#==================================================
ParNams <- names(ParDFlow1)
nParIhru <- ncol(ParDFlow1)

# length(as.numeric(as.matrix(ParDFlow1)))
# length(as.numeric(as.matrix(ParDFup1)))
