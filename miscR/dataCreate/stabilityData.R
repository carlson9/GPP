rm(list=ls())
setwd('~/Dropbox/GPSynth')
load('Data/stability.RData')

#outcome = BankZscore
#controls = Lernerindex, GDPgrowthannual, Realinterestrate

data = data.frame(country = table$Country, year = table$Year, zScore = table$BankZscore, lerner = table$Lernerindex, GDPgrowth = table$GDPgrowthannual, interest = table$Realinterestrate)
rm(table)

#1996-2017

# 01/01/1958
# Belgium
# France
# Germany
# Italy
# Luxembourg
# Netherlands
# 01/01/1973	Denmark
# Ireland
# United Kingdom (left on 31 January 2020)
# 
# 01/01/1981
# Greece
# 01/01/1986	Portugal
# Spain
# 01/01/1995	Austria
# Finland
# Sweden


# 01/05/2004	Cyprus
# Czechia
# Estonia
# Hungary
# Latvia
# Lithuania
# Malta
# Poland
# Slovakia
# Slovenia

indCyprus = which(data$country == 'Cyprus' & data$year >= 2004)
indCzechia = which(data$country == 'Czech Republic' & data$year >= 2004)
#Estonia not in data
indHungary = which(data$country == 'Hungary' & data$year >= 2004)
indLatvia = which(data$country == 'Latvia' & data$year >= 2004)
indLithuania = which(data$country == 'Lithuania' & data$year >= 2004)
#Malta not in data
#Poland not in data
indSlovakia = which(data$country == 'Slovak Republic' & data$year >= 2004)
indSlovenia = which(data$country == 'Slovenia' & data$year >= 2004)

# 01/01/2007	Bulgaria
# Romania

indBulgaria = which(data$country == 'Bulgaria' & data$year >= 2007)
indRomania = which(data$country == 'Romania' & data$year >= 2007)

# 01/07/2013	Croatia

indCroatia = which(data$country == 'Croatia' & data$year >= 2013)

data2 = data
data2[unlist(mget(paste0('ind', c('Cyprus',
                          'Czechia',
                          'Hungary',
                          'Latvia',
                          'Lithuania',
                          'Slovakia',
                          'Slovenia',
                          'Bulgaria',
                          'Romania',
                          'Croatia')))), 3:6] = NA


ystarIndCyprus = which(which(is.na(data2$zScore)) %in% indCyprus)
ystarIndCzechia = which(which(is.na(data2$zScore)) %in% indCzechia)
ystarIndHungary = which(which(is.na(data2$zScore)) %in% indHungary)
ystarIndLatvia = which(which(is.na(data2$zScore)) %in% indLatvia)
ystarIndLithuania = which(which(is.na(data2$zScore)) %in% indLithuania)
ystarIndSlovakia = which(which(is.na(data2$zScore)) %in% indSlovakia)
ystarIndSlovenia = which(which(is.na(data2$zScore)) %in% indSlovenia)
ystarIndBulgaria = which(which(is.na(data2$zScore)) %in% indBulgaria)
ystarIndRomania = which(which(is.na(data2$zScore)) %in% indRomania)
ystarIndCroatia = which(which(is.na(data2$zScore)) %in% indCroatia)

save(data, data2,
     ystarIndCyprus,
     ystarIndCzechia,
     ystarIndHungary,
     ystarIndLatvia,
     ystarIndLithuania,
     ystarIndSlovakia,
     ystarIndSlovenia,
     ystarIndBulgaria,
     ystarIndRomania,
     ystarIndCroatia,
     file = 'Data/stabilityWithInd.Rdata', version = 2)
