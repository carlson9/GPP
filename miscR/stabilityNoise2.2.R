rm(list=ls())

setwd('~/Dropbox/GPSynth')

# Load Data 
load("Data/stabilityWithInd.Rdata")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

xs1 = data2$lerner
xs2 = data2$GDPgrowth
xs3 = data2$interest
ys = data2$zScore
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
  N_countries = length(unique(data2$country)),
  N_years = length(unique(data2$year)),
  Country = as.numeric(as.factor(data2$country)),
  Year = as.numeric(as.factor(data2$year)),
  x1_in = as.numeric(scale(na.omit(xs1))),
  x2_in = as.numeric(scale(na.omit(xs2))),
  x3_in = as.numeric(scale(na.omit(xs3))),
  y_in = as.numeric(scale(as.numeric(na.omit(ys))))
)


compiled_model = rstan::stan_model('Code/FDINoise2.2.stan')

fit = sampling(compiled_model, data = to_stan,
               iter = 25000, chains = 2, cores = 2, seed = 1568,
               control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
save(fit, file = 'stabilityNoise2.2.Rdata')
#
load('stabilityNoise2.Rdata')

countries1 = c('Cyprus',
              'Czechia',
              'Hungary',
              'Latvia',
              'Lithuania',
              'Slovakia',
              'Slovenia',
              'Bulgaria',
              'Romania',
              'Croatia')

countries2 = c('Cyprus',
               'Czech Republic',
               'Hungary',
               'Latvia',
               'Lithuania',
               'Slovak Republic',
               'Slovenia',
               'Bulgaria',
               'Romania',
               'Croatia')

yearInd = c(rep(2004, 7),
            2007, 2007, 2013)

pdf('figures/stabilityNoise2.2.pdf', width = 6, height = 10)
par(mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8, mfrow = c(5,2))
# 
for(i in 1:10){
  plot(data$zScore[data$country == countries2[i]][order(data$year[data$country == countries2[i]])] ~ data$year[data$country == countries2[i]][order(data$year[data$country == countries2[i]])], type='l',
       xlab = 'Year', ylab = '(Predicted) z-Score', ylim = c(-10, 35), main = countries1[i])
  lines(data$year[data$country == countries2[i] & data$year >= yearInd[i]][order(data$year[data$country == countries2[i] & data$year >= yearInd[i]])], summary(fit)$summary[paste0('ystar[', get(paste0('ystarInd', countries1[i])), ']'), 'mean'][order(data$year[data$country == countries2[i] & data$year >= yearInd[i]])]*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
        type='l', col='red')
  lines(data$year[data$country == countries2[i] & data$year >= yearInd[i]][order(data$year[data$country == countries2[i] & data$year >= yearInd[i]])], summary(fit)$summary[paste0('ystar[', get(paste0('ystarInd', countries1[i])), ']'), '2.5%'][order(data$year[data$country == countries2[i] & data$year >= yearInd[i]])]*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
        type='l', col='red', lty = 3)
  lines(data$year[data$country == countries2[i] & data$year >= yearInd[i]][order(data$year[data$country == countries2[i] & data$year >= yearInd[i]])], summary(fit)$summary[paste0('ystar[', get(paste0('ystarInd', countries1[i])), ']'), '97.5%'][order(data$year[data$country == countries2[i] & data$year >= yearInd[i]])]*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))),
        type='l', col='red', lty = 3)
  legend('topleft', legend = c('z-Scores', 'Predicted z-Scores'), col = c('black', 'red'), lty=1, bty='n')
  
}
dev.off()
