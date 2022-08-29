# Visualization of dHRUM outputs
library(ggplot2)

setwd("C:/Users/Hossein/OneDrive/Desktop/R/BP_D_FG/GW")
getwd()
# BP_D_FG
# Paramters GW&SM together
dF_t$dat <- as.Date(with(dF_t, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
# dF_t$Month <- months(dF_t$dat)
# dF_t$Year <- format(dF_t$dat,format="%y")

{

SOISp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, SOIS])) + 
  geom_line() +
  xlab('Time') + 
  ylab('SM content') +
  ggtitle('BP_D_FG, SOIS, Applying GW')

SOIS <- SOISp + facet_wrap(~HruIds, ncol = 6) 
ggsave("SOIS.png", width = 25, height = 15)


GROSp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, GROS])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('GROS') + 
  ggtitle('BP_D_FG, GROS, Applying GW ')

GROS <- GROSp + facet_wrap(~HruIds, ncol = 6) 
ggsave("GROS.png", width = 25, height = 15)


EVBSp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, EVBS])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('EVBS') +
  ggtitle('BP_D_FG, EVBS, Applying GW ')
EVBS <- EVBSp + facet_wrap(~HruIds, ncol = 6)
ggsave("EVBS.png", width = 25, height = 15)


EVACp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, EVAC])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('EVAC') +
  ggtitle('BP_D_FG, EVAC, Applying GW ')
EVAC <- EVACp + facet_wrap(~HruIds, ncol = 6)
ggsave("EVAC.png", width = 25, height = 15)


EVASp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, EVAS])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('EVAS') +
  ggtitle('BP_D_FG, EVAS, Applying GW ')
EVAS <- EVASp + facet_wrap(~HruIds, ncol = 6)
ggsave("EVAS.png", width = 25, height = 15)


AETp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, AET])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('AET') +
  ggtitle('BP_D_FG, AET, Applying GW ')
AET <- AETp + facet_wrap(~HruIds, ncol = 6)
ggsave("AET.png", width = 25, height = 15)


SNOWp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, SNOW])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('SNOW') +
  ggtitle('BP_D_FG, SNOW, Applying GW ')
SNOW <- SNOWp + facet_wrap(~HruIds, ncol = 6)
ggsave("SNOW.png", width = 25, height = 15)


MELTp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, MELT])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('MELT') +
  ggtitle('BP_D_FG, MELT, Applying GW ')
MELT <- MELTp + facet_wrap(~HruIds, ncol = 6)
ggsave("MELT.png", width = 25, height = 15)


TROFp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, TROF])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('TROF') +
  ggtitle('BP_D_FG, TROF, Applying GW ')
TROF <- TROFp + facet_wrap(~HruIds, ncol = 6)
ggsave("TROF.png", width = 25, height = 15)


DIRRp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, DIRR])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('DIRR') +
  ggtitle('BP_D_FG, DIRR, Applying GW ')
DIRR <- DIRRp + facet_wrap(~HruIds, ncol = 6)
ggsave("DIRR.png", width = 25, height = 15)


STEFp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, STEF])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('STEF') +
  ggtitle('BP_D_FG, STEF, Applying GW ')
STEF <- STEFp + facet_wrap(~HruIds, ncol = 6)
ggsave("STEF.png", width = 25, height = 15)


CANFp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, CANF])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('CANF') +
  ggtitle('BP_D_FG, CANF, Applying GW ')
CANF <- CANFp + facet_wrap(~HruIds, ncol = 6)
ggsave("CANF.png", width = 25, height = 15)


CANSp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, CANS])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('CANS') +
  ggtitle('BP_D_FG, CANS, Applying GW ')
CANS <- CANSp + facet_wrap(~HruIds, ncol = 6)
ggsave("CANS.png", width = 25, height = 15)


STESp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, STES])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('STES') +
  ggtitle('BP_D_FG, STES, Applying GW ')
STES <- STESp + facet_wrap(~HruIds, ncol = 6)
ggsave("STES.png", width = 25, height = 15)



SURSp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, SURS])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('SURS') +
  ggtitle('BP_D_FG, SURS, Applying GW ')
SURS <- SURSp + facet_wrap(~HruIds, ncol = 6)
ggsave("SURS.png", width = 25, height = 15)


BASFp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, BASF])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('BASF') +
  ggtitle('BP_D_FG, BASF, Applying GW ')
BASF <- BASFp + facet_wrap(~HruIds, ncol = 6)
ggsave("BASF.png", width = 25, height = 15)


PERCp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, PERC])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('PERC') +
  ggtitle('BP_D_FG, PERC, Applying GW ')
PERC <- PERCp + facet_wrap(~HruIds, ncol = 6)
ggsave("PERC.png", width = 25, height = 15)


PREFp <- ggplot(dF_t, aes(dF_t[, dat], y = dF_t[, PREF])) + 
  geom_line() + 
  xlab('Time') + 
  ylab('PREF') +
  ggtitle('BP_D_FG, PREF, Applying GW ')
PREF <- PREFp + facet_wrap(~HruIds, ncol = 6)
ggsave("PREF.png", width = 25, height = 15)

}

names(dF_t)

