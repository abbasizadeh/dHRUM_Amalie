# loading Groundwater data (available for each HRU)
GW_list <- readRDS(file ="./inputs/Soil_input_data/SoilMoist_Groundwater/GW_BP_S_HRUs.rds")  


for(i in 1:38){
  # Calculating parameters for each HRU (using lumped model)
  SoilBP <- dtHrus[FID == i, ]
  NhrusBP <- nrow(SoilBP)
  nHrusBP <- NhrusBP # Change NhrusBP to nhursBP to be able to run Constrained_Parameters.r file
  
  Areas <- SoilBP$Area
  IdsHrus <- SoilBP$FID
  
  dhrusBP_D_FG <- initdHruModel(nHrusBP, Areas, IdsHrus)
  
  # setting up Groundwater and soil models 
  setGWtypeToAlldHrus(dHRUM_ptr = dhrusBP_D_FG, gwTypes = rep("LIN_RES", times=nHrusBP), hruIds = IdsHrus)
  setSoilStorTypeToAlldHrus(dHRUM_ptr = dhrusBP_D_FG, soilTypes = rep("PDM",times=nHrusBP), hruIds = IdsHrus)
  
  # Extracting P and T data for the i-th HRU
  dtaDF <- dtaDF_main[HruId == i, ]
  setPTInputsToDistdHRUM(dHRUM_ptr = dhrusBP_D_FG, dtaDF) 
  
  # parsDF <- readRDS(file ="D:/project/Setups_For_Dist_Model/outputs/BP_D_FG.rds")
  # parsDF <- parsDF[i, ]
  
  # Calculating potential evapotranspiration
  Hrus=as.character(c(dtaDF$HruId))
  lat = c(SoilBP$Lat)
  PetType <- as.vector(rep("HAMON", length(SoilBP$Lat)))
  calcPetToAllHrusDist(dHRUM_ptr = dhrusBP_D_FG, lat, PetType, HruIds = Hrus)
  
  # Groundwater time series for i-th HRU
  GW_TS <- data.table(GW_list[[i]])
  
  ParDFlow <- ParDFlow1[i, ]
  ParDFup <- ParDFup1[i, ]
  ParNams <- names(ParDFlow1)
  nParIhru <- ncol(ParDFlow1)
  rngGW <- range(GW_TS[HruId == i, date])
  
  
  mae = function(myPar){
    newmat <- as.data.frame(matrix(myPar, nrow = nHrusBP, ncol = nParIhru))
    names(newmat) <- ParNams
    setParsToDistdHRUM(dhrusBP_D_FG, newmat, FALSE)
    
    dta <- dHRUMrun(dHRUM_ptr = dhrusBP_D_FG)
    dF <-data.frame(dta$outDta)
    dF$X28 <- dta$Ids
    names(dF) <- dta$VarsNams
    dF$date <- as.Date(with(dF, paste(YEAR, MONTH, DAY, sep="-")), "%Y-%m-%d")
    dF[c("YEAR", "MONTH", "DAY")] <- NULL
    dF <- as.data.table(dF)
    
    GWranege_dHRUM  <- dF[date >= rngGW[1] & date <= rngGW[2], .(date, GROS)]
    # GWranege_dHRUM <- GWranege_dHRUM[ , stdGROS := scale(GROS)]
    GW <- merge(GW_TS[HruId == i,], GWranege_dHRUM, by = "date")
    # CorGW <- cor(GW[, GW_level], GW[, stdGROS])
    CorGW <- cor(GW[, GW_level], GW[, GROS])
    mymae =as.double(1 - CorGW)
    
  }
  
  itermaxW = 50 
  decntr <- DEoptim.control(VTR = 0, strategy = 2, bs = FALSE, NP = 160,
                            itermax = itermaxW, CR = 0.95, F = 0.9, trace = TRUE,
                            initialpop = NULL, storepopfrom = itermaxW + 1,
                            storepopfreq = 2, p = 0.2, c = 0, reltol = sqrt(.Machine$double.eps),
                            steptol = itermaxW)
  
  u = DEoptim(lower=as.numeric(as.matrix(ParDFlow)),
              upper=as.numeric(as.matrix(ParDFup)), fn = mae, control = decntr)
  
  ParBestVec <- as.numeric(u$optim$bestmem)
  ParBestGW <- as.data.frame(matrix(ParBestVec, nrow = nHrusBP, ncol = nParIhru))
  names(ParBestGW) <- ParNams
  
  
# saveRDS(ParBestGW, paste0("./outputs/SM&GW_CalibratedParams/Pars_BP_S_FG_GW_", i))

}









