library(data.table)
library(ggplot2)

setwd('D:/Project/Amalie_Paper')
getwd()

# the directory of the downloaded csv file
Files <- list.files('./GWdata') 
FilePath <- paste0('./GWdata/', Files)

# reading csv files and storing them into a list (GW_TSs) 
GW_TSs <- lapply(FilePath, read.csv)
GW_TSs <- lapply(GW_TSs, as.data.table)

# function for decomposing Time column into date and time columns
# Time column format: 2022-05-10 12:51:22
FUN = function(x){
  x$date <- as.Date(x$Time)
  x$Time <- format(as.POSIXct(x$Time), format = '%H:%M')
  return(x)}

# extracting the sensors' IDs
Name <- substr(x = Files, start = 1, stop = 15)

for(i in 1:length(Files)){
  GW_TSs[[i]] <- FUN(GW_TSs[[i]])
  GW_TSs[[i]]$day <- lubridate::day(GW_TSs[[i]]$date)
  GW_TSs[[i]]$month <- lubridate::month(GW_TSs[[i]]$date)
  GW_TSs[[i]]$year <- lubridate::year(GW_TSs[[i]]$date)
  GW_TSs[[i]]$ID <- rep(Name[i], length(GW_TSs[[i]]$day))
  
  names(GW_TSs[[i]]) <- c('Time', 'value', 'date', 'day', 'month', 'year','ID')
    }

# the same procedure for '9/15/2021 11:11' format
Files2 <- list.files('./GWdata2') 
FilePath2 <- paste0('./GWdata2/', Files2)
GW_TSs2 <- lapply(FilePath2, read.csv)
GW_TSs2 <- lapply(GW_TSs2, as.data.table)

FUN2 = function(x){
  x$date <- as.Date(x$Time, format = "%m/%d/%y")
  x$Time <- format(as.POSIXct(x$Time, tryFormats = "%m/%d/%Y %H:%M"), 
                   format = '%H:%M')
  return(x)}

Name2 <- substr(x = Files2, start = 1, stop = 15)

for(i in 1:length(Files2)){
  GW_TSs[[length(Files)+i]] <- FUN2(GW_TSs2[[i]])
  GW_TSs[[length(Files)+i]]$day <- lubridate::day(GW_TSs[[length(Files)+i]]$date)
  GW_TSs[[length(Files)+i]]$month <- lubridate::month(GW_TSs[[length(Files)+i]]$date) 
  GW_TSs[[length(Files)+i]]$year <- lubridate::year(GW_TSs[[length(Files)+i]]$date)
  GW_TSs[[length(Files)+i]]$ID <- rep(Name[i], length(GW_TSs[[length(Files)+i]]$day))
  names(GW_TSs[[length(Files)+i]]) <- c('Time', 'value', 'date', 'day', 'month', 'year','ID') 
}

# checking the columns' configuration
GW_TSs[[1]]
GW_TSs[[5]]
GW_TSs[[10]]
GW_TSs[[15]]
GW_TSs[[25]]
GW_TSs[[30]]
GW_TSs[[35]]
GW_TSs[[40]]
GW_TSs[[45]]


# calculating daily Groundwater level (mean value of sub-daily time scale)
GW_TSs_daily <- list()
for (i in 1:length(GW_TSs)) {
  GW_TSs_daily[[i]] <- GW_TSs[[i]][, mean(value), by = .(day, month, year, ID)]
  GW_TSs_daily[[i]]$date <- as.Date(with(GW_TSs_daily[[i]], paste(year, month, day,sep="-")), "%Y-%m-%d")
  GW_TSs_daily[[i]]$day <- NULL
  GW_TSs_daily[[i]]$month <- NULL
  GW_TSs_daily[[i]]$year <- NULL
  names(GW_TSs_daily[[i]]) <- c('ID', 'value', 'date')
}

# plotting 
dta <- data.frame(GW_TSs_daily[[1]])
for (i in 2:45){
  dta <- rbind(dta ,data.frame(GW_TSs_daily[[i]])) 
}

ggplot(data = dta) + geom_line(aes(x = date, y = value)) + facet_wrap(~ID, ncol = 5) 

ggplot(data = dta) + geom_line(aes(x = date, y = value, color = ID)) + 
  theme(legend.position="none")

for(j in 1:45){
  GW_TS <- data.table(GW_TSs_daily[[j]])
  p <- ggplot() + geom_line(data = GW_TS, aes(x = date, y = value)) +
    ggtitle(paste0('Sensor ', as.character(j)))
  ggsave(filename = paste0(j, '.png'), plot = p, path = "C:/Users/Hossein/OneDrive/Desktop/R/Sensors/New sensors/",
         width = 30, height = 15, units = 'cm')
}


# Files <- gsub('-2022-08-28 ', '-', Files)