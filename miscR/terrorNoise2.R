rm(list=ls())

setwd('~/Dropbox/GPSynth')

# Load Data 
load("Data/terrorTrends.Rdata")
d = trends
rm(trends)

d = d[!is.na(d$iso3n), ]
d = d[!is.na(d$country), ]

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

xs1 = d$UNEMPSA_
xs1[d$country == 'Afghanistan' & d$year > 2001] = NA
xs2 = d$SE.XPD.TOTL.GD.ZS
xs2[d$country == 'Afghanistan' & d$year > 2001] = NA
xs3 = d$EFindex
xs3[d$country == 'Afghanistan' & d$year > 2001] = NA
X = d$conflict
ys = d$nAttacks
ys[d$country == 'Afghanistan' & d$year > 2001] = NA
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
  y_obs_ind = which(!is.na(ys)),
  N_countries = length(unique(d$country)),
  N_years = length(unique(d$year)),
  Country = as.numeric(as.factor(d$country)),
  Year = as.numeric(as.factor(d$year)),
  x1_in = as.numeric(scale(na.omit(xs1))),
  x2_in = as.numeric(scale(na.omit(xs2))),
  x3_in = as.numeric(scale(na.omit(xs3))),
  y_in = as.numeric(na.omit(ys)),
  X = as.matrix(X),
  K = 1,
  reciprocal_phi_scale = 5
)


# compiled_model = rstan::stan_model('Code/NBNoise2.stan')
# 
# fit = sampling(compiled_model, data = to_stan,
#                iter = 12500, chains = 2, cores = 2, seed = 156,
#                control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
# save(fit, file = 'terrorNoise2.Rdata')

load('terrorNoise2.Rdata')

pdf('figures/terrorNoise2.pdf', width = 6, height = 4)
par(mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8)
plot(d$nAttacks[d$country == 'Afghanistan'][order(d$year[d$country == 'Afghanistan'])] ~ d$year[d$country == 'Afghanistan'][order(d$year[d$country == 'Afghanistan'])], type='l',
     xlab = 'Year', ylab = '(Predicted) No. of Attacks')

lines(2002:2018, summary(fit)$summary[paste0('ystar[', 1:17, ']'), 'mean'][order(d$year[is.na(ys)])],
      type='l', col='red')
lines(2002:2018, summary(fit)$summary[paste0('ystar[', 1:17, ']'), '2.5%'][order(d$year[is.na(ys)])],
      type='l', col='red',lty=2)
lines(2002:2018, summary(fit)$summary[paste0('ystar[', 1:17, ']'), '97.5%'][order(d$year[is.na(ys)])],
      type='l', col='red',lty=2)
legend('topleft', legend = c('No. of Attacks', 'Predicted No. of Attacks'), col = c('black', 'red'), lty=1, bty='n')
dev.off()
