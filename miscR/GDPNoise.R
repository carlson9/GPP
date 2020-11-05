rm(list=ls())

setwd('~/Dropbox/GPSynth')

# Load Data 
load('Data/d.RData')
X = cbind(d$invest, d$school, d$ind)

X = apply(X, 2, scale)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

xs1 = d$infrate
xs1[d$country == 'West Germany' & d$year > 1989] = NA
xs2 = d$trade
xs2[d$country == 'West Germany' & d$year > 1989] = NA
ys = d$gdp
ys[d$country == 'West Germany' & d$year > 1989] = NA
to_stan = list(
  x1_N_obs = sum(!is.na(xs1)),
  x1_N_miss = sum(is.na(xs1)),
  x1_miss_ind = which(is.na(xs1)),
  x2_N_obs = sum(!is.na(xs2)),
  x2_N_miss = sum(is.na(xs2)),
  x2_miss_ind = which(is.na(xs2)),
  y_N_obs = sum(!is.na(ys)),
  y_N_miss = sum(is.na(ys)),
  y_miss_ind = which(is.na(ys)),
  N_countries = length(unique(d$country)),
  N_years = length(unique(d$year)),
  Country = as.numeric(as.factor(d$country)),
  Year = as.numeric(as.factor(d$year)),
  x1_in = as.numeric(scale(na.omit(xs1))),
  x2_in = as.numeric(scale(na.omit(xs2))),
  y_in = as.numeric(scale(as.numeric(na.omit(ys)))),
  K = ncol(X),
  X = X
)


compiled_model = rstan::stan_model('Code/Noise2.stan')

fit = sampling(compiled_model, data = to_stan,
               iter = 25000, chains = 2, cores = 2, seed = 156,
               control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
save(fit, file = 'GDPNoise2.Rdata')
load('GDPNoise2.Rdata')

pdf('figures/germanyNoise2.pdf', width = 6, height = 4)
par(mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8)
plot(d$gdp[d$country=='West Germany'] ~
     d$year[d$country=='West Germany'],
     type = 'l', xlab = 'Year', ylab = '(Predicted) GDP',
     ylim = c(0,37500))
lines(1990:2003, summary(fit)$summary[paste0('ystar[', 1:14, ']'), 'mean']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
      type='l', col='red')
lines(1990:2003, summary(fit)$summary[paste0('ystar[', 1:14, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
      type='l', col='red', lty=2)
lines(1990:2003, summary(fit)$summary[paste0('ystar[', 1:14, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
      type='l', col='red', lty=2)
legend('topleft', legend = c('GDP', 'Predicted GDP'), col = c('black', 'red'), lty = 1, bty = 'n')
dev.off()
#
summary(summary(fit)$summary[,'Rhat'])
