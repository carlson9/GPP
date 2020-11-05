rm(list=ls())
library(rstan)

setwd('~/Dropbox/GPSynth')
load("Data/d.RData")
countries = unique(d$country)
countries = countries[countries != 'West Germany']

within = c()
for(i in 1:16){
  load(paste0(i, 'Placebo/', countries[i], 'Noise2Placebo.Rdata'))
  
  d2 = d[!(d$country == 'West Germany' & d$year > 1989),]
  ys = d2$gdp
  ys = d2$gdp
  totest = ys[d2$country == countries[i] & d2$year > 1989]
  
  within = c(within, totest > summary(fit)$summary[paste0('ystar[', 1:14, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
               totest < summary(fit)$summary[paste0('ystar[', 1:14, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))))
  
  
}

mean(within) #0.9642857

            