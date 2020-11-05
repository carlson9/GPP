rm(list=ls())

setwd('~/Dropbox/GPSynth')

library(parallel)

cl <- makeCluster(16, type='FORK', useXDR = FALSE)
parLapply(cl, 1:16, function(i) {
  # Load Data 
  load("Data/d.RData")
  dir.create(paste0(i,'Placebo'))
  setwd(paste0(i,'Placebo'))
  library(rstan)
  model_code = "data {
  int<lower=0> x1_N_obs;
  int<lower=0> x1_N_miss;
  int<lower=0> x1_miss_ind[x1_N_miss]; //indexes for missing x
  int<lower=0> x2_N_obs;
  int<lower=0> x2_N_miss;
  int<lower=0> x2_miss_ind[x2_N_miss]; //indexes for missing x
  int<lower=0> y_N_obs;
  int<lower=0> y_N_miss;
  int<lower=0> y_miss_ind[y_N_miss];
  int<lower=0> N_countries;
  int<lower=0> N_years;
  int<lower=0> Country[x1_N_obs + x1_N_miss];
  int<lower=0> Year[x1_N_obs + x1_N_miss];
  vector[x1_N_obs] x1_in;
  vector[x2_N_obs] x2_in;
  vector[y_N_obs] y_in;
  int<lower=0> K; //number of predictors that do not require imputation
  matrix[x1_N_obs + x1_N_miss, K] X; //matrix of predictors that do not require imputation
}
  transformed data {
  int N;
  real years[N_years];
  vector[N_years] years_real;
  N = x1_N_obs + x1_N_miss;
  for (t in 1:N_years) years[t] = t;
  for (t in 1:N_years) years_real[t] = (t - mean(to_vector(years)))/sd(to_vector(years));
  }
  parameters{
  real x1b0;
  real x2b0;
  real yb0;
  vector[N_countries] x1_country_re;
  vector[N_years] x1_year_re;
  real<lower=0> x1_year_sig;
  real<lower=0> x1_country_sig;
  real<lower=0> x1_sigma;
  matrix[N_years, N_countries] x1_GP_std;
  real<lower=0> x1_length_GP_long;
  real<lower=0> x1_sigma_GP_long;
  vector[x1_N_miss] x1_miss;
  vector[N_countries] x2_country_re;
  vector[N_years] x2_year_re;
  real<lower=0> x2_year_sig;
  real<lower=0> x2_country_sig;
  real<lower=0> x2_sigma;
  matrix[N_years, N_countries] x2_GP_std;
  real<lower=0> x2_length_GP_long;
  real<lower=0> x2_sigma_GP_long;
  vector[x2_N_miss] x2_miss;
  vector[y_N_miss] y_miss;
  real b1;
  real b2;
  real<lower=0> sigma_t;
  matrix[N_years, N_countries] y_GP_std;
  vector[N_countries] y_country_re;
  vector[N_years] y_year_re;
  real<lower=0> y_year_sig;
  real<lower=0> y_country_sig;
  vector[K] B; //coefficients for unimputed data
  real<lower=0> x1nug;
  real<lower=0> x2nug;
  real<lower=0> ynug;
  real<lower=0> sig_sq;
  vector[N] noise;
  }
  transformed parameters{
  matrix[N_years,N_countries] x1_GP_term;
  matrix[N_years,N_countries] x2_GP_term;
  matrix[N_years,N_countries] y_GP_term;
  {
  matrix[N_years, N_years] x1_cov;
  matrix[N_years, N_years] x1_L_cov;
  x1_cov = cov_exp_quad(years, x1_sigma_GP_long,
  x1_length_GP_long);
  for (year in 1:N_years) x1_cov[year, year] += x1nug;
  x1_L_cov = cholesky_decompose(x1_cov);
  x1_GP_term = x1_L_cov * x1_GP_std;
  }
  {
  matrix[N_years, N_years] x2_cov;
  matrix[N_years, N_years] x2_L_cov;
  x2_cov = cov_exp_quad(years, x2_sigma_GP_long,
  x2_length_GP_long);
  for (year in 1:N_years) x2_cov[year, year] += x2nug;
  x2_L_cov = cholesky_decompose(x2_cov);
  x2_GP_term = x2_L_cov * x2_GP_std;
  }
  {
  matrix[N_years, N_years] y_cov;
  matrix[N_years, N_years] y_L_cov;
  y_cov = sigma_t + years_real*years_real';
  for (year in 1:N_years) y_cov[year, year] += ynug;
  y_L_cov = cholesky_decompose(y_cov);
  y_GP_term = y_L_cov * y_GP_std;
  }


  }
model {
  vector[N] xz1;
  vector[N] xz2;
  int counter1_in;
  int counter1_out;
  int counter2_in;
  int counter2_out;
  int y_counter_in;
  int y_counter_out;
  vector[N] yz;

  x1nug ~ exponential(1);
  x2nug ~ exponential(1);
  ynug ~ exponential(1);

  sigma_t ~ gamma(2, .5);
  to_vector(y_GP_std) ~ normal(0, 1);
  
  to_vector(x1_GP_std) ~ normal(0, 1);
  x1_length_GP_long ~ weibull(30,8);
  x1_sigma_GP_long ~ inv_gamma(1,1);
  
  x1_country_sig ~ gamma(2, .5);
  x1_year_sig ~ gamma(2, .5);
  x1_year_re ~ normal(0, x1_year_sig);
  x1_country_re ~ normal(0, x1_country_sig);
  x1_sigma ~ gamma(2, .5);
  
  to_vector(x2_GP_std) ~ normal(0, 1);
  x2_length_GP_long ~ weibull(30,8);
  x2_sigma_GP_long ~ inv_gamma(1,1);
  
  x2_country_sig ~ gamma(2, .5);
  x2_year_sig ~ gamma(2, .5);
  x2_year_re ~ normal(0, x2_year_sig);
  x2_country_re ~ normal(0, x2_country_sig);
  x2_sigma ~ gamma(2, .5);
  
  y_country_sig ~ gamma(2, .5);
  y_year_sig ~ gamma(2, .5);
  y_year_re ~ normal(0, y_year_sig);
  y_country_re ~ normal(0, y_country_sig);
  
  
  x1b0 ~ normal(0,3);
  x2b0 ~ normal(0,3);
  yb0 ~ normal(0,3);
  
  B ~ normal(0,3);
  
  counter1_in = 1;
  counter1_out = 1;
  for(n in 1:N){
    if(counter1_out <= x1_N_miss){
      if(n == x1_miss_ind[counter1_out]){
        xz1[n] = x1_miss[counter1_out];
        counter1_out += 1;
      }else{
        xz1[n] = x1_in[counter1_in];
        counter1_in += 1;
      }
    }else{
      xz1[n] = x1_in[counter1_in];
      counter1_in += 1;
    }
    xz1[n] ~ normal(x1b0 + x1_year_re[Year[n]] + x1_country_re[Country[n]] +
                      x1_GP_term[Year[n], Country[n]], x1_sigma);
  }
  
  counter2_in = 1;
  counter2_out = 1;
  for(n in 1:N){
    if(counter2_out <= x2_N_miss){
      if(n == x2_miss_ind[counter2_out]){
        xz2[n] = x2_miss[counter2_out];
        counter2_out += 1;
      }else{
        xz2[n] = x2_in[counter2_in];
        counter2_in += 1;
      }
    }else{
      xz2[n] = x2_in[counter2_in];
      counter2_in += 1;
    }
    xz2[n] ~ normal(x2b0 + x2_year_re[Year[n]] + x2_country_re[Country[n]] +
                      x2_GP_term[Year[n], Country[n]], x2_sigma);
  }
  
  y_counter_in = 1;
  y_counter_out = 1;
  for(n in 1:N){
    if(y_counter_out <= y_N_miss){
      if(n == y_miss_ind[y_counter_out]){
        yz[n] = y_miss[y_counter_out] + noise[n];
        y_counter_out += 1;
      }else{
        yz[n] = y_in[y_counter_in] + noise[n];
        y_counter_in += 1;
      }
    }else{
      yz[n] = y_in[y_counter_in] + noise[n];
      y_counter_in += 1;
    }
  }
  
  
  sig_sq ~ exponential(1);
  
  b1 ~ normal(0,3);
  b2 ~ normal(0,3);

  noise ~ normal(0, .25);
  
  for (n in 1:N) yz[n] ~ normal(yb0 + xz1[n]*b1 + xz2[n]*b2 + X[n,]*B + y_year_re[Year[n]] + y_country_re[Country[n]] +
                                  y_GP_term[Year[n], Country[n]], sig_sq);
}
"
  # compiled_model = rstan::stan_model(',,/Code/xyMissMultiDotProdOptimScaleCrazy2.stan')
  countries = unique(d$country)
  countries = countries[countries != 'West Germany']
  
  d = d[!(d$country == 'West Germany' & d$year > 1989),]

  X = cbind(d$invest, d$school, d$ind)
  X = apply(X, 2, scale)
  

  
  
  
  
  set.seed(i)
  xs1 = d$infrate
  xs1[d$country == countries[i] & d$year > 1989] = NA
  xs2 = d$trade
  xs2[d$country == countries[i] & d$year > 1989] = NA
  ys = d$gdp
  ys[d$country == countries[i] & d$year > 1989] = NA
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
  
  fit = stan(model_code = model_code, model_name = countries[i], data = to_stan,
                 iter = 50000, chains = 1, cores = 1, seed = i,
                 control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
  save(fit, file = paste0(countries[i], 'NoisePlacebo.Rdata'))
  rm(fit)
  setwd('..')
})
stopCluster(cl)
