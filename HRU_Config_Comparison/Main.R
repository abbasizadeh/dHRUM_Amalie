
library(ggplot2)
library(reshape2)

setwd("D:/project/Amalie_Paper")
getwd()
{
  BP_D_GW <- readRDS(file ="./Rscripts/dHRUM_setup/outputs/HeatMapData/BP_D_GW.rds")
  dFBP_D_GW <- data.frame(BP_D_GW$outDta)
  names(dFBP_D_GW) <- BP_D_GW$VarsNams
  dFBP_D_GW$DTM <- as.Date(with(dFBP_D_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_D_GW)
  
  #########
  
  BP_S_GW <- readRDS(file ="./Rscripts/dHRUM_setup/outputs/HeatMapData/BP_S_GW.rds")
  dFBP_S_GW <- data.frame(BP_S_GW$outDta)
  names(dFBP_S_GW) <- BP_S_GW$VarsNams
  dFBP_S_GW$DTM <- as.Date(with(dFBP_S_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_S_GW)
  
  ##############
  
  KL_D_GW <- readRDS(file ="./Rscripts/dHRUM_setup/outputs/HeatMapData/KL_D_GW.rds")
  dFKL_D_GW <- data.frame(KL_D_GW$outDta)
  names(dFKL_D_GW) <- KL_D_GW$VarsNams
  dFKL_D_GW$DTM <- as.Date(with(dFKL_D_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_D_GW)
  
  
  KL_S_GW <- readRDS(file ="./Rscripts/dHRUM_setup/outputs/HeatMapData/KL_S_GW.rds")
  dFKL_S_GW  <- data.frame(KL_S_GW$outDta)
  names(dFKL_S_GW) <- KL_S_GW$VarsNams
  dFKL_S_GW$DTM <- as.Date(with(dFKL_S_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_S_GW)
  
}


# GROS Merging dataframes
{
 
  GROSBP <- merge(dFBP_D_GW[c("DTM", "GROS")], dFBP_S_GW[c("DTM", "GROS")], by = "DTM")
  GROSKL <- merge(dFKL_D_GW[c("DTM", "GROS")], dFKL_S_GW[c("DTM", "GROS")], by = "DTM")
  GROS_GW <- merge(GROSBP, GROSKL, by = "DTM")
  names(GROS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # GROS_GW
}


# SOIS Merging dataframes
{
  
  SOISBP <- merge(dFBP_D_GW[c("DTM", "SOIS")], dFBP_S_GW[c("DTM", "SOIS")], by = "DTM")
  SOISKL <- merge(dFKL_D_GW[c("DTM", "SOIS")], dFKL_S_GW[c("DTM", "SOIS")], by = "DTM")
  SOIS_GW <- merge(SOISBP, SOISKL, by = "DTM")
  names(SOIS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SOIS_GW
  
}


# Interception Merging dataframes
{
  
  INTSBP <- merge(dFBP_D_GW[c("DTM", "INTS")], dFBP_S_GW[c("DTM", "INTS")], by = "DTM")
  INTSKL <- merge(dFKL_D_GW[c("DTM", "INTS")], dFKL_S_GW[c("DTM", "INTS")], by = "DTM")
  INTS_GW <- merge(INTSBP, INTSKL, by = "DTM")
  names(INTS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # INTS_GW

}


# SURS Merging dataframes
{
  
  SURSBP <- merge(dFBP_D_GW[c("DTM", "SURS")], dFBP_S_GW[c("DTM", "SURS")], by = "DTM")
  SURSKL <- merge(dFKL_D_GW[c("DTM", "SURS")], dFKL_S_GW[c("DTM", "SURS")], by = "DTM")
  SURS_GW <- merge(SURSBP, SURSKL, by = "DTM")
  names(SURS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SURS_GW
  
}


# TOTR Merging dataframes
{

  TOTRBP <- merge(dFBP_D_GW[c("DTM", "TOTR")], dFBP_S_GW[c("DTM", "TOTR")], by = "DTM")
  TOTRKL <- merge(dFKL_D_GW[c("DTM", "TOTR")], dFKL_S_GW[c("DTM", "TOTR")], by = "DTM")
  TOTR_GW <- merge(TOTRBP, TOTRKL, by = "DTM")
  names(TOTR_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # TOTR_GW
  
}


# BASF Merging dataframes
{
 
  BASFBP <- merge(dFBP_D_GW[c("DTM", "BASF")], dFBP_S_GW[c("DTM", "BASF")], by = "DTM")
  BASFKL <- merge(dFKL_D_GW[c("DTM", "BASF")], dFKL_S_GW[c("DTM", "BASF")], by = "DTM")
  BASF_GW <- merge(BASFBP, BASFKL, by = "DTM")
  names(BASF_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # BASF_GW
  
  
}



# DIRR Merging dataframes
{
  
  DIRRBP <- merge(dFBP_D_GW[c("DTM", "DIRR")], dFBP_S_GW[c("DTM", "DIRR")], by = "DTM")
  DIRRKL <- merge(dFKL_D_GW[c("DTM", "DIRR")], dFKL_S_GW[c("DTM", "DIRR")], by = "DTM")
  DIRR_GW <- merge(DIRRBP, DIRRKL, by = "DTM")
  names(DIRR_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # DIRR_GW
  
  
}

# PERC Merging dataframes
{
  
  PERCBP <- merge(dFBP_D_GW[c("DTM", "PERC")], dFBP_S_GW[c("DTM", "PERC")], by = "DTM")
  PERCKL <- merge(dFKL_D_GW[c("DTM", "PERC")], dFKL_S_GW[c("DTM", "PERC")], by = "DTM")
  PERC_GW <- merge(PERCBP, PERCKL, by = "DTM")
  names(PERC_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PERC_GW
  
}


# SNOW Merging dataframes
{
  
  SNOWBP <- merge(dFBP_D_GW[c("DTM", "SNOW")], dFBP_S_GW[c("DTM", "SNOW")], by = "DTM")
  SNOWKL <- merge(dFKL_D_GW[c("DTM", "SNOW")], dFKL_S_GW[c("DTM", "SNOW")], by = "DTM")
  SNOW_GW <- merge(SNOWBP, SNOWKL, by = "DTM")
  names(SNOW_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SNOW_GW
  
}


# PET Merging dataframes
{
  
  PETBP <- merge(dFBP_D_GW[c("DTM", "PET")], dFBP_S_GW[c("DTM", "PET")], by = "DTM")
  PETKL <- merge(dFKL_D_GW[c("DTM", "PET")], dFKL_S_GW[c("DTM", "PET")], by = "DTM")
  PET_GW <- merge(PETBP, PETKL, by = "DTM")
  names(PET_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PET_GW

}


# MELT Merging dataframes
{
  
  MELTBP <- merge(dFBP_D_GW[c("DTM", "MELT")], dFBP_S_GW[c("DTM", "MELT")], by = "DTM")
  MELTKL <- merge(dFKL_D_GW[c("DTM", "MELT")], dFKL_S_GW[c("DTM", "MELT")], by = "DTM")
  MELT_GW <- merge(MELTBP, MELTKL, by = "DTM")
  names(MELT_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # MELT_GW
  
  
}


# Get lower triangle of the correlation matrix
get_lower_tri<-function(corMtx){
  corMtx[upper.tri(corMtx)] <- NA
  return(corMtx)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(corMtx){
  corMtx[lower.tri(corMtx)]<- NA
  return(corMtx)
}

