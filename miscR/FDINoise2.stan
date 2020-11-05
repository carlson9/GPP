data {
  int<lower=0> x1_N_obs;
  int<lower=0> x1_N_miss;
  int<lower=0> x1_miss_ind[x1_N_miss]; //indexes for missing x
  int<lower=0> x2_N_obs;
  int<lower=0> x2_N_miss;
  int<lower=0> x2_miss_ind[x2_N_miss]; //indexes for missing x
  int<lower=0> x3_N_obs;
  int<lower=0> x3_N_miss;
  int<lower=0> x3_miss_ind[x3_N_miss]; //indexes for missing x
  int<lower=0> y_N_obs;
  int<lower=0> y_N_miss;
  int<lower=0> y_miss_ind[y_N_miss];
  int<lower=0> N_countries;
  int<lower=0> N_years;
  int<lower=0> Country[x1_N_obs + x1_N_miss];
  int<lower=0> Year[x1_N_obs + x1_N_miss];
  vector[x1_N_obs] x1_in;
  vector[x2_N_obs] x2_in;
  vector[x3_N_obs] x3_in;
  vector[y_N_obs] y_in;
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
  real x3b0;
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
  vector[N_countries] x3_country_re;
  vector[N_years] x3_year_re;
  real<lower=0> x3_year_sig;
  real<lower=0> x3_country_sig;
  real<lower=0> x3_sigma;
  matrix[N_years, N_countries] x3_GP_std;
  real<lower=0> x3_length_GP_long;
  real<lower=0> x3_sigma_GP_long;
  vector[x3_N_miss] x3_miss;
  vector[y_N_miss] y_miss;
  real b1;
  real b2;
  real b3;
  real<lower=0> sig_sq;
  real<lower=0> sigma_t;
  matrix[N_years, N_countries] y_GP_std;
  vector[N_countries] y_country_re;
  vector[N_years] y_year_re;
  real<lower=0> y_year_sig;
  real<lower=0> y_country_sig;
}
transformed parameters{
  matrix[N_years,N_countries] x1_GP_term;
  matrix[N_years,N_countries] x2_GP_term;
  matrix[N_years,N_countries] x3_GP_term;
  matrix[N_years,N_countries] y_GP_term;
    {
      matrix[N_years, N_years] x1_cov;
      matrix[N_years, N_years] x1_L_cov;
      x1_cov = cov_exp_quad(years, x1_sigma_GP_long,
        x1_length_GP_long);
      for (year in 1:N_years) x1_cov[year, year] += 1e-12;
      x1_L_cov = cholesky_decompose(x1_cov);
      x1_GP_term = x1_L_cov * x1_GP_std;
    }
    {
      matrix[N_years, N_years] x2_cov;
      matrix[N_years, N_years] x2_L_cov;
      x2_cov = cov_exp_quad(years, x2_sigma_GP_long,
        x2_length_GP_long);
      for (year in 1:N_years) x2_cov[year, year] += 1e-12;
      x2_L_cov = cholesky_decompose(x2_cov);
      x2_GP_term = x2_L_cov * x2_GP_std;
    }
    {
      matrix[N_years, N_years] x3_cov;
      matrix[N_years, N_years] x3_L_cov;
      x3_cov = cov_exp_quad(years, x3_sigma_GP_long,
        x3_length_GP_long);
      for (year in 1:N_years) x3_cov[year, year] += 1e-12;
      x3_L_cov = cholesky_decompose(x3_cov);
      x3_GP_term = x3_L_cov * x3_GP_std;
    }

    {
      matrix[N_years, N_years] y_cov;
      matrix[N_years, N_years] y_L_cov;
      y_cov = sigma_t + years_real*years_real';
      for (year in 1:N_years) y_cov[year, year] += 1e-12;
      y_L_cov = cholesky_decompose(y_cov);
      y_GP_term = y_L_cov * y_GP_std;
    }

}
model {
  vector[N] xz1;
  vector[N] xz2;
  vector[N] xz3;
  int counter1_in;
  int counter1_out;
  int counter2_in;
  int counter2_out;
  int counter3_in;
  int counter3_out;
  int y_counter_in;
  int y_counter_out;
  vector[N] yz;

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
  
  to_vector(x3_GP_std) ~ normal(0, 1);
  x3_length_GP_long ~ weibull(30,8);
  x3_sigma_GP_long ~ inv_gamma(1,1);

  x3_country_sig ~ gamma(2, .5);
  x3_year_sig ~ gamma(2, .5);
  x3_year_re ~ normal(0, x3_year_sig);
  x3_country_re ~ normal(0, x3_country_sig);
  x3_sigma ~ gamma(2, .5);

  
  y_country_sig ~ gamma(2, .5);
  y_year_sig ~ gamma(2, .5);
  y_year_re ~ normal(0, y_year_sig);
  y_country_re ~ normal(0, y_country_sig);


  x1b0 ~ normal(0,3);
  x2b0 ~ normal(0,3);
  x3b0 ~ normal(0,3);
  yb0 ~ normal(0,3);
  
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

  counter3_in = 1;
  counter3_out = 1;
  for(n in 1:N){
    if(counter3_out <= x3_N_miss){
      if(n == x3_miss_ind[counter3_out]){
        xz3[n] = x3_miss[counter3_out];
        counter3_out += 1;
      }else{
        xz3[n] = x3_in[counter3_in];
        counter3_in += 1;
      }
    }else{
        xz3[n] = x3_in[counter3_in];
        counter3_in += 1;
      }
    xz3[n] ~ normal(x3b0 + x3_year_re[Year[n]] + x3_country_re[Country[n]] +
      x3_GP_term[Year[n], Country[n]], x3_sigma);
    }

    
  y_counter_in = 1;
  y_counter_out = 1;
  for(n in 1:N){
    if(y_counter_out <= y_N_miss){
      if(n == y_miss_ind[y_counter_out]){
        yz[n] = y_miss[y_counter_out];
        y_counter_out += 1;
      }else{
        yz[n] = y_in[y_counter_in];
        y_counter_in += 1;
      }
    }else{
        yz[n] = y_in[y_counter_in];
        y_counter_in += 1;
      }
  }


    
    sig_sq ~ inv_gamma(1,1);
    b1 ~ normal(0,3);
    b2 ~ normal(0,3);
    b3 ~ normal(0,3);
    
    for (n in 1:N) yz[n] ~ normal(yb0 + xz1[n]*b1 + xz2[n]*b2 + xz3[n]*b3 + y_year_re[Year[n]] + y_country_re[Country[n]] +
      y_GP_term[Year[n], Country[n]], sig_sq);
}
generated quantities {
  vector[y_N_miss] ystar;
  for(nm in 1:y_N_miss) ystar[nm] = normal_rng(y_miss[nm], .25);
}

