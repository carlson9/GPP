rm(list=ls())
setwd('/home/david/Dropbox/GPSynth')

library(WDI)


#23 June 2016
#right before quarter 3

#Political Risk Services
#Corruption perception index - transparency international
#Political risk services, world economic forum
#log of population (country and market size)
#log of gdp per capita (consumption potential)
#trade/GDP (openness)
#unemployment (labor availability)
#

data = read.csv('Data/fdiOut.csv')
data = data[data$LOCATION != 'EU',]
data = data[data$LOCATION != 'WLD',]
data = data[data$LOCATION != 'OECD',]
library(countrycode)
data$iso2c = countrycode(data$LOCATION, 'iso3c', 'iso2c')
data$year = as.numeric(substr(data$TIME, start = 1, stop = 4)) - 1 #lagged

#unemployment and population (lagged)
unpop = WDI(country = unique(data$iso2c), indicator = c('SL.UEM.TOTL.ZS', 'SP.POP.TOTL'), start = 2012, end = 2017)
data = merge(data, unpop, by = c('iso2c', 'year'))

#GDP quarterly
gdpData = read.csv('Data/gdpQuarter.csv')
gdpData$numTime = as.numeric(gdpData$TIME) #automatically lags because of the unique levels of the data sets
data$numTime = as.numeric(data$TIME)
data = merge(data, gdpData, by = c('numTime', 'LOCATION'))
colnames(data)[colnames(data) == 'Value.x'] = 'fdi'
colnames(data)[colnames(data) == 'Value.y'] = 'gdp'
save(data, file='Data/FDIout.Rdata')

time = as.numeric(data$TIME)
treatTime = time[data$TIME == '2016-Q4'][1]
