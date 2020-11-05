rm(list=ls())

setwd('~/Dropbox/GPSynth')

# Load Data 
load("Data/FDIout.Rdata")
d = data
rm(data)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

xs1 = d$SL.UEM.TOTL.ZS
xs1[d$country == 'United Kingdom' & d$numTime > 15] = NA
xs2 = d$SP.POP.TOTL
xs2[d$country == 'United Kingdom' & d$numTime > 15] = NA
xs3 = d$gdp
xs3[d$country == 'United Kingdom' & d$numTime > 15] = NA
ys = d$fdi
ys[d$country == 'United Kingdom' & d$numTime > 15] = NA
to_stan = list(
  x1_N_obs = sum(!is.na(xs1)),
  x1_N_miss = sum(is.na(xs1)),
  x1_miss_ind = which(is.na(xs1)),
  x2_N_obs = sum(!is.na(xs2)),
  x2_N_miss = sum(is.na(xs2)),
  x2_miss_ind = which(is.na(xs2)),
  x3_N_obs = sum(!is.na(xs3)),
  x3_N_miss = sum(is.na(xs3)),
  x3_miss_ind = which(is.na(xs3)),
  y_N_obs = sum(!is.na(ys)),
  y_N_miss = sum(is.na(ys)),
  y_miss_ind = which(is.na(ys)),
  N_countries = length(unique(d$country)),
  N_years = length(unique(d$numTime)),
  Country = as.numeric(as.factor(d$country)),
  Year = as.numeric(as.factor(d$numTime)),
  x1_in = as.numeric(scale(na.omit(xs1))),
  x2_in = as.numeric(scale(na.omit(xs2))),
  x3_in = as.numeric(scale(na.omit(xs3))),
  y_in = as.numeric(scale(as.numeric(na.omit(ys))))
)


compiled_model = rstan::stan_model('Code/FDINoise2.stan')

fit = sampling(compiled_model, data = to_stan,
               iter = 25000, chains = 2, cores = 2, seed = 1568,
               control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
save(fit, file = 'fdiOutNoise2.Rdata')
# 
load('fdiOutNoise2.Rdata')
pdf('figures/fdiOutNoise2.pdf', width = 6, height = 4)
par(mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8)
plot(d$fdi[d$country == 'United Kingdom'][order(d$numTime[d$country == 'United Kingdom'])] ~ d$numTime[d$country == 'United Kingdom'][order(d$numTime[d$country == 'United Kingdom'])], type='l',
     xaxt = 'n', xlab = 'Time', ylab = '(Predicted) FDI Outflows')
axis(1, at = 1:24, labels = d$TIME.x[d$country == 'United Kingdom'][order(d$numTime[d$country == 'United Kingdom'])])
# summary(fit)$summary[paste0('y_miss[', 1:sum(is.na(ys)), ']'),]
lines(16:24, summary(fit)$summary[paste0('y_miss[', 1:9, ']'), 'mean'][order(d$numTime[is.na(ys)])]*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
      type='l', col='red')
lines(16:24, summary(fit)$summary[paste0('y_miss[', 1:9, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
      type='l', col='red',lty=2)
lines(16:24, summary(fit)$summary[paste0('y_miss[', 1:9, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
      type='l', col='red',lty=2)
legend('bottomright', legend = c('FDI Outflows', 'Predicted FDI Outflows'), col = c('black', 'red'), lty=1, bty='n')
dev.off()
