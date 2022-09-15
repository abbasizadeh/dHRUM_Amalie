
library(ggplot2)
library(reshape2)

{
  BP_D_Exp <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_D_Exp.rds")
  dFBP_D_Exp <- data.frame(BP_D_Exp$outDta)
  names(dFBP_D_Exp) <- BP_D_Exp$VarsNams
  dFBP_D_Exp$DTM <- as.Date(with(dFBP_D_Exp, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_D_Exp)
  
  BP_D_GWSM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_D_GW&SM.rds")
  dFBP_D_GWSM <- data.frame(BP_D_GWSM$outDta)
  names(dFBP_D_GWSM) <- BP_D_GWSM$VarsNams
  dFBP_D_GWSM$DTM <- as.Date(with(dFBP_D_GWSM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_D_GWSM)
  
  BP_D_GW <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_D_GW.rds")
  dFBP_D_GW <- data.frame(BP_D_GW$outDta)
  names(dFBP_D_GW) <- BP_D_GW$VarsNams
  dFBP_D_GW$DTM <- as.Date(with(dFBP_D_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_D_GW)
  
  BP_D_SM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_D_SM.rds")
  dFBP_D_SM <- data.frame(BP_D_SM$outDta)
  names(dFBP_D_SM) <- BP_D_SM$VarsNams
  dFBP_D_SM$DTM <- as.Date(with(dFBP_D_SM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_D_SM)
  
  #########
  BP_S_Exp <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_S_Exp.rds")
  dFBP_S_Exp <- data.frame(BP_S_Exp$outDta)
  names(dFBP_S_Exp) <- BP_S_Exp$VarsNams
  dFBP_S_Exp$DTM <- as.Date(with(dFBP_S_Exp, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_S_Exp)
  
  BP_S_GWSM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_S_GW&SM.rds")
  dFBP_S_GWSM <- data.frame(BP_S_GWSM$outDta)
  names(dFBP_S_GWSM) <- BP_S_GWSM$VarsNams
  dFBP_S_GWSM$DTM <- as.Date(with(dFBP_S_GWSM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_S_GWSM)
  
  BP_S_GW <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_S_GW.rds")
  dFBP_S_GW <- data.frame(BP_S_GW$outDta)
  names(dFBP_S_GW) <- BP_S_GW$VarsNams
  dFBP_S_GW$DTM <- as.Date(with(dFBP_S_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_S_GW)
  
  BP_S_SM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/BP_S_SM.rds")
  dFBP_S_SM <- data.frame(BP_S_SM$outDta)
  names(dFBP_S_SM) <- BP_S_SM$VarsNams
  dFBP_S_SM$DTM <- as.Date(with(dFBP_S_SM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(BP_S_SM)
  
  ##############
  KL_D_Exp <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_D_Exp.rds")
  dFKL_D_Exp <- data.frame(KL_D_Exp$outDta)
  names(dFKL_D_Exp) <- KL_D_Exp$VarsNams
  dFKL_D_Exp$DTM <- as.Date(with(dFKL_D_Exp, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_D_Exp)
  
  KL_D_GWSM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_D_GW&SM.rds")
  dFKL_D_GWSM <- data.frame(KL_D_GWSM$outDta)
  names(dFKL_D_GWSM) <- KL_D_GWSM$VarsNams
  dFKL_D_GWSM$DTM <- as.Date(with(dFKL_D_GWSM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_D_GWSM)
  
  KL_D_GW <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_D_GW.rds")
  dFKL_D_GW <- data.frame(KL_D_GW$outDta)
  names(dFKL_D_GW) <- KL_D_GW$VarsNams
  dFKL_D_GW$DTM <- as.Date(with(dFKL_D_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_D_GW)
  
  KL_D_SM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_D_SM.rds")
  dFKL_D_SM <- data.frame(KL_D_SM$outDta)
  names(dFKL_D_SM) <- KL_D_SM$VarsNams
  dFKL_D_SM$DTM <- as.Date(with(dFKL_D_SM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_D_SM)
  
  
  KL_S_Exp <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_S_Exp.rds")
  dFKL_S_Exp <- data.frame(KL_S_Exp$outDta)
  names(dFKL_S_Exp) <- KL_S_Exp$VarsNams
  dFKL_S_Exp$DTM <- as.Date(with(dFKL_S_Exp, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_S_Exp)
  
  KL_S_GWSM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_S_GW&SM.rds")
  dFKL_S_GWSM <- data.frame(KL_S_GWSM$outDta)
  names(dFKL_S_GWSM) <- KL_S_GWSM$VarsNams
  dFKL_S_GWSM$DTM <- as.Date(with(dFKL_S_GWSM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_S_GWSM)
  
  KL_S_GW <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_S_GW.rds")
  dFKL_S_GW  <- data.frame(KL_S_GW$outDta)
  names(dFKL_S_GW) <- KL_S_GW$VarsNams
  dFKL_S_GW$DTM <- as.Date(with(dFKL_S_GW, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_S_GW)
  
  KL_S_SM <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/HeatMap/KL_S_SM.rds")
  dFKL_S_SM  <- data.frame(KL_S_SM$outDta)
  names(dFKL_S_SM) <- KL_S_SM$VarsNams
  dFKL_S_SM$DTM <- as.Date(with(dFKL_S_SM, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
  rm(KL_S_SM)
}




# GROS Merging dataframes
{
  GROSBP <- merge(dFBP_D_Exp[c("DTM", "GROS")], dFBP_S_Exp[c("DTM", "GROS")], by = "DTM")
  GROSKL <- merge(dFKL_D_Exp[c("DTM", "GROS")], dFKL_S_Exp[c("DTM", "GROS")], by = "DTM")
  GROS_Exp <- merge(GROSBP, GROSKL, by = "DTM")
  names(GROS_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  GROSBP <- merge(dFBP_D_GWSM[c("DTM", "GROS")], dFBP_S_GWSM[c("DTM", "GROS")], by = "DTM")
  GROSKL <- merge(dFKL_D_GWSM[c("DTM", "GROS")], dFKL_S_GWSM[c("DTM", "GROS")], by = "DTM")
  GROS_GWSM <- merge(GROSBP, GROSKL, by = "DTM")
  names(GROS_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # GROS_GWSM
  
  
  GROSBP <- merge(dFBP_D_GW[c("DTM", "GROS")], dFBP_S_GW[c("DTM", "GROS")], by = "DTM")
  GROSKL <- merge(dFKL_D_GW[c("DTM", "GROS")], dFKL_S_GW[c("DTM", "GROS")], by = "DTM")
  GROS_GW <- merge(GROSBP, GROSKL, by = "DTM")
  names(GROS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # GROS_GW
  
  
  GROSBP <- merge(dFBP_D_SM[c("DTM", "GROS")], dFBP_S_SM[c("DTM", "GROS")], by = "DTM")
  GROSKL <- merge(dFKL_D_SM[c("DTM", "GROS")], dFKL_S_SM[c("DTM", "GROS")], by = "DTM")
  GROS_SM <- merge(GROSBP, GROSKL, by = "DTM")
  names(GROS_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # GROS_SM
}


# SOIS Merging dataframes
{
  SOISBP <- merge(dFBP_D_Exp[c("DTM", "SOIS")], dFBP_S_Exp[c("DTM", "SOIS")], by = "DTM")
  SOISKL <- merge(dFKL_D_Exp[c("DTM", "SOIS")], dFKL_S_Exp[c("DTM", "SOIS")], by = "DTM")
  SOIS_Exp <- merge(SOISBP, SOISKL, by = "DTM")
  names(SOIS_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  SOISBP <- merge(dFBP_D_GWSM[c("DTM", "SOIS")], dFBP_S_GWSM[c("DTM", "SOIS")], by = "DTM")
  SOISKL <- merge(dFKL_D_GWSM[c("DTM", "SOIS")], dFKL_S_GWSM[c("DTM", "SOIS")], by = "DTM")
  SOIS_GWSM <- merge(SOISBP, SOISKL, by = "DTM")
  names(SOIS_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SOIS_GWSM
  
  
  SOISBP <- merge(dFBP_D_GW[c("DTM", "SOIS")], dFBP_S_GW[c("DTM", "SOIS")], by = "DTM")
  SOISKL <- merge(dFKL_D_GW[c("DTM", "SOIS")], dFKL_S_GW[c("DTM", "SOIS")], by = "DTM")
  SOIS_GW <- merge(SOISBP, SOISKL, by = "DTM")
  names(SOIS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SOIS_GW
  
  
  SOISBP <- merge(dFBP_D_SM[c("DTM", "SOIS")], dFBP_S_SM[c("DTM", "SOIS")], by = "DTM")
  SOISKL <- merge(dFKL_D_SM[c("DTM", "SOIS")], dFKL_S_SM[c("DTM", "SOIS")], by = "DTM")
  SOIS_SM <- merge(SOISBP, SOISKL, by = "DTM")
  names(SOIS_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SOIS_SM
}



# Interception Merging dataframes
{
  INTSBP <- merge(dFBP_D_Exp[c("DTM", "INTS")], dFBP_S_Exp[c("DTM", "INTS")], by = "DTM")
  INTSKL <- merge(dFKL_D_Exp[c("DTM", "INTS")], dFKL_S_Exp[c("DTM", "INTS")], by = "DTM")
  INTS_Exp <- merge(INTSBP, INTSKL, by = "DTM")
  names(INTS_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  INTSBP <- merge(dFBP_D_GWSM[c("DTM", "INTS")], dFBP_S_GWSM[c("DTM", "INTS")], by = "DTM")
  INTSKL <- merge(dFKL_D_GWSM[c("DTM", "INTS")], dFKL_S_GWSM[c("DTM", "INTS")], by = "DTM")
  INTS_GWSM <- merge(INTSBP, INTSKL, by = "DTM")
  names(INTS_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # INTS_GWSM
  
  
  INTSBP <- merge(dFBP_D_GW[c("DTM", "INTS")], dFBP_S_GW[c("DTM", "INTS")], by = "DTM")
  INTSKL <- merge(dFKL_D_GW[c("DTM", "INTS")], dFKL_S_GW[c("DTM", "INTS")], by = "DTM")
  INTS_GW <- merge(INTSBP, INTSKL, by = "DTM")
  names(INTS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # INTS_GW
  
  
  INTSBP <- merge(dFBP_D_SM[c("DTM", "INTS")], dFBP_S_SM[c("DTM", "INTS")], by = "DTM")
  INTSKL <- merge(dFKL_D_SM[c("DTM", "INTS")], dFKL_S_SM[c("DTM", "INTS")], by = "DTM")
  INTS_SM <- merge(INTSBP, INTSKL, by = "DTM")
  names(INTS_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # INTS_SM
}


# SURS Merging dataframes
{
  SURSBP <- merge(dFBP_D_Exp[c("DTM", "SURS")], dFBP_S_Exp[c("DTM", "SURS")], by = "DTM")
  SURSKL <- merge(dFKL_D_Exp[c("DTM", "SURS")], dFKL_S_Exp[c("DTM", "SURS")], by = "DTM")
  SURS_Exp <- merge(SURSBP, SURSKL, by = "DTM")
  names(SURS_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  SURSBP <- merge(dFBP_D_GWSM[c("DTM", "SURS")], dFBP_S_GWSM[c("DTM", "SURS")], by = "DTM")
  SURSKL <- merge(dFKL_D_GWSM[c("DTM", "SURS")], dFKL_S_GWSM[c("DTM", "SURS")], by = "DTM")
  SURS_GWSM <- merge(SURSBP, SURSKL, by = "DTM")
  names(SURS_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SURS_GWSM
  
  
  SURSBP <- merge(dFBP_D_GW[c("DTM", "SURS")], dFBP_S_GW[c("DTM", "SURS")], by = "DTM")
  SURSKL <- merge(dFKL_D_GW[c("DTM", "SURS")], dFKL_S_GW[c("DTM", "SURS")], by = "DTM")
  SURS_GW <- merge(SURSBP, SURSKL, by = "DTM")
  names(SURS_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SURS_GW
  
  
  SURSBP <- merge(dFBP_D_SM[c("DTM", "SURS")], dFBP_S_SM[c("DTM", "SURS")], by = "DTM")
  SURSKL <- merge(dFKL_D_SM[c("DTM", "SURS")], dFKL_S_SM[c("DTM", "SURS")], by = "DTM")
  SURS_SM <- merge(SURSBP, SURSKL, by = "DTM")
  names(SURS_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SURS_SM
}


# TOTR Merging dataframes
{
  TOTRBP <- merge(dFBP_D_Exp[c("DTM", "TOTR")], dFBP_S_Exp[c("DTM", "TOTR")], by = "DTM")
  TOTRKL <- merge(dFKL_D_Exp[c("DTM", "TOTR")], dFKL_S_Exp[c("DTM", "TOTR")], by = "DTM")
  TOTR_Exp <- merge(TOTRBP, TOTRKL, by = "DTM")
  names(TOTR_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  TOTRBP <- merge(dFBP_D_GWSM[c("DTM", "TOTR")], dFBP_S_GWSM[c("DTM", "TOTR")], by = "DTM")
  TOTRKL <- merge(dFKL_D_GWSM[c("DTM", "TOTR")], dFKL_S_GWSM[c("DTM", "TOTR")], by = "DTM")
  TOTR_GWSM <- merge(TOTRBP, TOTRKL, by = "DTM")
  names(TOTR_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # TOTR_GWSM
  
  
  TOTRBP <- merge(dFBP_D_GW[c("DTM", "TOTR")], dFBP_S_GW[c("DTM", "TOTR")], by = "DTM")
  TOTRKL <- merge(dFKL_D_GW[c("DTM", "TOTR")], dFKL_S_GW[c("DTM", "TOTR")], by = "DTM")
  TOTR_GW <- merge(TOTRBP, TOTRKL, by = "DTM")
  names(TOTR_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # TOTR_GW
  
  
  TOTRBP <- merge(dFBP_D_SM[c("DTM", "TOTR")], dFBP_S_SM[c("DTM", "TOTR")], by = "DTM")
  TOTRKL <- merge(dFKL_D_SM[c("DTM", "TOTR")], dFKL_S_SM[c("DTM", "TOTR")], by = "DTM")
  TOTR_SM <- merge(TOTRBP, TOTRKL, by = "DTM")
  names(TOTR_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # TOTR_SM
}


# BASF Merging dataframes
{
  BASFBP <- merge(dFBP_D_Exp[c("DTM", "BASF")], dFBP_S_Exp[c("DTM", "BASF")], by = "DTM")
  BASFKL <- merge(dFKL_D_Exp[c("DTM", "BASF")], dFKL_S_Exp[c("DTM", "BASF")], by = "DTM")
  BASF_Exp <- merge(BASFBP, BASFKL, by = "DTM")
  names(BASF_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  BASFBP <- merge(dFBP_D_GWSM[c("DTM", "BASF")], dFBP_S_GWSM[c("DTM", "BASF")], by = "DTM")
  BASFKL <- merge(dFKL_D_GWSM[c("DTM", "BASF")], dFKL_S_GWSM[c("DTM", "BASF")], by = "DTM")
  BASF_GWSM <- merge(BASFBP, BASFKL, by = "DTM")
  names(BASF_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # BASF_GWSM
  
  
  BASFBP <- merge(dFBP_D_GW[c("DTM", "BASF")], dFBP_S_GW[c("DTM", "BASF")], by = "DTM")
  BASFKL <- merge(dFKL_D_GW[c("DTM", "BASF")], dFKL_S_GW[c("DTM", "BASF")], by = "DTM")
  BASF_GW <- merge(BASFBP, BASFKL, by = "DTM")
  names(BASF_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # BASF_GW
  
  
  BASFBP <- merge(dFBP_D_SM[c("DTM", "BASF")], dFBP_S_SM[c("DTM", "BASF")], by = "DTM")
  BASFKL <- merge(dFKL_D_SM[c("DTM", "BASF")], dFKL_S_SM[c("DTM", "BASF")], by = "DTM")
  BASF_SM <- merge(BASFBP, BASFKL, by = "DTM")
  names(BASF_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # BASF_SM
}



# DIRR Merging dataframes
{
  DIRRBP <- merge(dFBP_D_Exp[c("DTM", "DIRR")], dFBP_S_Exp[c("DTM", "DIRR")], by = "DTM")
  DIRRKL <- merge(dFKL_D_Exp[c("DTM", "DIRR")], dFKL_S_Exp[c("DTM", "DIRR")], by = "DTM")
  DIRR_Exp <- merge(DIRRBP, DIRRKL, by = "DTM")
  names(DIRR_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  DIRRBP <- merge(dFBP_D_GWSM[c("DTM", "DIRR")], dFBP_S_GWSM[c("DTM", "DIRR")], by = "DTM")
  DIRRKL <- merge(dFKL_D_GWSM[c("DTM", "DIRR")], dFKL_S_GWSM[c("DTM", "DIRR")], by = "DTM")
  DIRR_GWSM <- merge(DIRRBP, DIRRKL, by = "DTM")
  names(DIRR_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # DIRR_GWSM
  
  
  DIRRBP <- merge(dFBP_D_GW[c("DTM", "DIRR")], dFBP_S_GW[c("DTM", "DIRR")], by = "DTM")
  DIRRKL <- merge(dFKL_D_GW[c("DTM", "DIRR")], dFKL_S_GW[c("DTM", "DIRR")], by = "DTM")
  DIRR_GW <- merge(DIRRBP, DIRRKL, by = "DTM")
  names(DIRR_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # DIRR_GW
  
  
  DIRRBP <- merge(dFBP_D_SM[c("DTM", "DIRR")], dFBP_S_SM[c("DTM", "DIRR")], by = "DTM")
  DIRRKL <- merge(dFKL_D_SM[c("DTM", "DIRR")], dFKL_S_SM[c("DTM", "DIRR")], by = "DTM")
  DIRR_SM <- merge(DIRRBP, DIRRKL, by = "DTM")
  names(DIRR_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # DIRR_SM
}

# PERC Merging dataframes
{
  PERCBP <- merge(dFBP_D_Exp[c("DTM", "PERC")], dFBP_S_Exp[c("DTM", "PERC")], by = "DTM")
  PERCKL <- merge(dFKL_D_Exp[c("DTM", "PERC")], dFKL_S_Exp[c("DTM", "PERC")], by = "DTM")
  PERC_Exp <- merge(PERCBP, PERCKL, by = "DTM")
  names(PERC_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  PERCBP <- merge(dFBP_D_GWSM[c("DTM", "PERC")], dFBP_S_GWSM[c("DTM", "PERC")], by = "DTM")
  PERCKL <- merge(dFKL_D_GWSM[c("DTM", "PERC")], dFKL_S_GWSM[c("DTM", "PERC")], by = "DTM")
  PERC_GWSM <- merge(PERCBP, PERCKL, by = "DTM")
  names(PERC_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PERC_GWSM
  
  
  PERCBP <- merge(dFBP_D_GW[c("DTM", "PERC")], dFBP_S_GW[c("DTM", "PERC")], by = "DTM")
  PERCKL <- merge(dFKL_D_GW[c("DTM", "PERC")], dFKL_S_GW[c("DTM", "PERC")], by = "DTM")
  PERC_GW <- merge(PERCBP, PERCKL, by = "DTM")
  names(PERC_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PERC_GW
  
  
  PERCBP <- merge(dFBP_D_SM[c("DTM", "PERC")], dFBP_S_SM[c("DTM", "PERC")], by = "DTM")
  PERCKL <- merge(dFKL_D_SM[c("DTM", "PERC")], dFKL_S_SM[c("DTM", "PERC")], by = "DTM")
  PERC_SM <- merge(PERCBP, PERCKL, by = "DTM")
  names(PERC_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PERC_SM
}


# SNOW Merging dataframes
{
  SNOWBP <- merge(dFBP_D_Exp[c("DTM", "SNOW")], dFBP_S_Exp[c("DTM", "SNOW")], by = "DTM")
  SNOWKL <- merge(dFKL_D_Exp[c("DTM", "SNOW")], dFKL_S_Exp[c("DTM", "SNOW")], by = "DTM")
  SNOW_Exp <- merge(SNOWBP, SNOWKL, by = "DTM")
  names(SNOW_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  SNOWBP <- merge(dFBP_D_GWSM[c("DTM", "SNOW")], dFBP_S_GWSM[c("DTM", "SNOW")], by = "DTM")
  SNOWKL <- merge(dFKL_D_GWSM[c("DTM", "SNOW")], dFKL_S_GWSM[c("DTM", "SNOW")], by = "DTM")
  SNOW_GWSM <- merge(SNOWBP, SNOWKL, by = "DTM")
  names(SNOW_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SNOW_GWSM
  
  
  SNOWBP <- merge(dFBP_D_GW[c("DTM", "SNOW")], dFBP_S_GW[c("DTM", "SNOW")], by = "DTM")
  SNOWKL <- merge(dFKL_D_GW[c("DTM", "SNOW")], dFKL_S_GW[c("DTM", "SNOW")], by = "DTM")
  SNOW_GW <- merge(SNOWBP, SNOWKL, by = "DTM")
  names(SNOW_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SNOW_GW
  
  
  SNOWBP <- merge(dFBP_D_SM[c("DTM", "SNOW")], dFBP_S_SM[c("DTM", "SNOW")], by = "DTM")
  SNOWKL <- merge(dFKL_D_SM[c("DTM", "SNOW")], dFKL_S_SM[c("DTM", "SNOW")], by = "DTM")
  SNOW_SM <- merge(SNOWBP, SNOWKL, by = "DTM")
  names(SNOW_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # SNOW_SM
}


# PET Merging dataframes
{
  PETBP <- merge(dFBP_D_Exp[c("DTM", "PET")], dFBP_S_Exp[c("DTM", "PET")], by = "DTM")
  PETKL <- merge(dFKL_D_Exp[c("DTM", "PET")], dFKL_S_Exp[c("DTM", "PET")], by = "DTM")
  PET_Exp <- merge(PETBP, PETKL, by = "DTM")
  names(PET_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  PETBP <- merge(dFBP_D_GWSM[c("DTM", "PET")], dFBP_S_GWSM[c("DTM", "PET")], by = "DTM")
  PETKL <- merge(dFKL_D_GWSM[c("DTM", "PET")], dFKL_S_GWSM[c("DTM", "PET")], by = "DTM")
  PET_GWSM <- merge(PETBP, PETKL, by = "DTM")
  names(PET_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PET_GWSM
  
  
  PETBP <- merge(dFBP_D_GW[c("DTM", "PET")], dFBP_S_GW[c("DTM", "PET")], by = "DTM")
  PETKL <- merge(dFKL_D_GW[c("DTM", "PET")], dFKL_S_GW[c("DTM", "PET")], by = "DTM")
  PET_GW <- merge(PETBP, PETKL, by = "DTM")
  names(PET_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PET_GW
  
  
  PETBP <- merge(dFBP_D_SM[c("DTM", "PET")], dFBP_S_SM[c("DTM", "PET")], by = "DTM")
  PETKL <- merge(dFKL_D_SM[c("DTM", "PET")], dFKL_S_SM[c("DTM", "PET")], by = "DTM")
  PET_SM <- merge(PETBP, PETKL, by = "DTM")
  names(PET_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # PET_SM
}


# MELT Merging dataframes
{
  MELTBP <- merge(dFBP_D_Exp[c("DTM", "MELT")], dFBP_S_Exp[c("DTM", "MELT")], by = "DTM")
  MELTKL <- merge(dFKL_D_Exp[c("DTM", "MELT")], dFKL_S_Exp[c("DTM", "MELT")], by = "DTM")
  MELT_Exp <- merge(MELTBP, MELTKL, by = "DTM")
  names(MELT_Exp) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  
  
  
  MELTBP <- merge(dFBP_D_GWSM[c("DTM", "MELT")], dFBP_S_GWSM[c("DTM", "MELT")], by = "DTM")
  MELTKL <- merge(dFKL_D_GWSM[c("DTM", "MELT")], dFKL_S_GWSM[c("DTM", "MELT")], by = "DTM")
  MELT_GWSM <- merge(MELTBP, MELTKL, by = "DTM")
  names(MELT_GWSM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # MELT_GWSM
  
  
  MELTBP <- merge(dFBP_D_GW[c("DTM", "MELT")], dFBP_S_GW[c("DTM", "MELT")], by = "DTM")
  MELTKL <- merge(dFKL_D_GW[c("DTM", "MELT")], dFKL_S_GW[c("DTM", "MELT")], by = "DTM")
  MELT_GW <- merge(MELTBP, MELTKL, by = "DTM")
  names(MELT_GW) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # MELT_GW
  
  
  MELTBP <- merge(dFBP_D_SM[c("DTM", "MELT")], dFBP_S_SM[c("DTM", "MELT")], by = "DTM")
  MELTKL <- merge(dFKL_D_SM[c("DTM", "MELT")], dFKL_S_SM[c("DTM", "MELT")], by = "DTM")
  MELT_SM <- merge(MELTBP, MELTKL, by = "DTM")
  names(MELT_SM) <- c("DTM", "BP_D", "BP_S", "KL_D", "KL_S")
  # MELT_SM
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

