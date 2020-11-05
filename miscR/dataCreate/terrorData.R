rm(list=ls())

setwd('~/Dropbox/GPSynth')

# Load Data 
data = read.csv("Data/globalterrorismdb_0919dist.csv")
data = data.frame(year = data$iyear, natlty_txt = data$natlty1_txt, natlty = data$natlty1)
library(tidyr)
trends = crossing(year = data$year, natlty_txt = data$natlty_txt)
trends$nAttacks = 0
for(yr in unique(trends$year)){
  for(cn in unique(trends$natlty_txt)){
    trends$nAttacks[trends$year == yr & trends$natlty_txt == cn] = sum(data$natlty_txt == cn & data$year == yr)
  }
}

trends$natlty = NULL
for(cn in unique(trends$natlty_txt)){
  trends$natlty[trends$natlty_txt == cn] = data$natlty[data$natlty_txt == cn][1]
}


af = trends[trends$natlty_txt == 'Afghanistan',]
plot(af$nAttacks ~ af$year, type = 'l')
abline(v=2001)

save(trends, file = 'Data/terrorTrends.Rdata')

load('Data/terrorTrends.Rdata')
conflict = read.csv('Data/ucdp-prio-acd-191.csv')

conflict = conflict[conflict$year >= 1970, c('gwno_loc', 'year')]
unique(conflict$gwno_loc)
gwno = strsplit(as.character(conflict$gwno_loc), ', ')
gwno_len = unlist(lapply(gwno, length))
conflict = data.frame(year = rep(conflict$year, gwno_len), gwno = unlist(gwno))

library(countrycode)
conflict$iso3n = countrycode(conflict$gwno, 'gwn', 'iso3n')
trends$iso3n = countrycode(trends$natlty_txt, 'country.name', 'iso3n')

hold = cbind(conflict$iso3n, conflict$year)

trends$conflict = 0
for(cn in unique(trends$iso3n)){
  for(yr in unique(trends$year)){
    trends$conflict[trends$iso3n == cn & trends$year == yr] = min(1, sum(hold[,1] == cn & hold[,2] == yr, na.rm = T))
  }
}

library(WDI)

controls = WDI(indicator = c('UNEMPSA_', 'SE.XPD.TOTL.GD.ZS'), start = 1970, end = 2018, extra = T)
controls$iso3n = countrycode(controls$iso3c, 'iso3c', 'iso3n')
trends = merge(trends, controls, by = c('iso3n', 'year'), all.x = T)

HIEF = read.csv('Data/HIEF_data.csv')
HIEF$iso3n = countrycode(HIEF$Country, 'country.name', 'iso3n')

colnames(HIEF)[2] = 'year'

trends = merge(trends, HIEF, by = c('iso3n', 'year'), all.x = T)

save(trends, file = 'Data/terrorTrends.Rdata')
