#' Writes Stan code for GPP model
#'
#' Returns string of Stan code that can be run to estimate the GPP. 
#'
#' @param noise The desired amount of artificial noise to add to the model.
#' @param ncov The number of covariates to include in the model. 
#'
#' @return A string of Stan code that can be run with \code{\link{runMod.R}} 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' @examples
#' 
#' writeMod(noise = 0.25, ncov = 2)
#' 
#' @seealso \code{\link{plotGPPfit.R}} \code{\link{runMod.R}} \code{\link{GPP.R}} \code{\link{autoconverge.R}}
#' @rdname writeMod
#' @aliases writeMod,ANY-method
#' @export
setGeneric(name="writeMod",
           def=function(noise, ncov)
           {standardGeneric("writeMod")}
)

#' @export
setMethod(f="writeMod",
          definition=function(noise, ncov){
            modstring = 'data { \n'
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'int<lower=0> x', i,'_N_obs; \n')
              modstring = paste0(modstring, 'int<lower=0> x', i,'_N_miss; \n')
              modstring = paste0(modstring, 'int<lower=0> x', i,'_N_miss_ind[x', i, '_N_miss]; \n')
              modstring = paste0(modstring, 'vector[x', i, '_N_obs] x', i, '_in;\n')
            }
            
            modstring = paste0(modstring,'int<lower=0> y_N_obs; \n
   int<lower=0> y_N_miss;
   int<lower=0> y_miss_ind[y_N_miss];
   int<lower=0> N_countries;
   int<lower=0> N_years;
   int<lower=0> Country[x1_N_obs + x1_N_miss];
   int<lower=0> Year[x1_N_obs + x1_N_miss];
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
   parameters{ \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'real x', i, 'b0;')
              modstring = paste0(modstring, 'vector[N_countries] x', i,'_country_re;')
              modstring = paste0(modstring, 'vector[N_years] x' ,i,'_year_re;')
              modstring = paste0(modstring, 'real<lower=0> x',i,'_year_sig;')
              modstring = paste0(modstring, 'real<lower=0> x',i,'_country_sig;')
              modstring = paste0(modstring, 'real<lower=0> x',i,'_sigma;')
              modstring = paste0(modstring, 'matrix[N_years, N_countries] x',i,'_GP_std;')
              modstring = paste0(modstring, 'real<lower=0> x',i,'_length_GP_long;')
              modstring = paste0(modstring, 'real<lower=0> x',i,'_sigma_GP_long;')
              modstring = paste0(modstring, 'vector[x',i,'_N_miss] x',i,'_miss;')
              modstring = paste0(modstring, 'real b',i,';')
              modstring = paste0(modstring, 'real<lower=0> x',i,'nug;')
            }
            modstring = paste0(modstring, '\n real yb0;
  vector[y_N_miss] y_miss;
  real<lower=0> sigma_t;
  matrix[N_years, N_countries] y_GP_std;
  vector[N_countries] y_country_re;
  vector[N_years] y_year_re;
  real<lower=0> y_year_sig;
  real<lower=0> y_country_sig;
  real<lower=0> ynug;
  real<lower=0> sig_sq;
  }
  transformed parameters{ \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'matrix[N_years,N_countries] x',i,'_GP_term; \n')
            }
            modstring = paste0(modstring, 'matrix[N_years,N_countries] y_GP_term;\n')
            for (i in 1:ncov){
              modstring = paste0(modstring, '{
                       matrix[N_years, N_years] x',i,'_cov; \n')
              modstring = paste0(modstring, 'matrix[N_years, N_years] x',i,'_L_cov; \n')
              modstring = paste0(modstring, 'x', i,'_cov = cov_exp_quad(years, x',i,'1_sigma_GP_long,
                          x',i,'_length_GP_long); \n')
              modstring = paste0(modstring, 'for (year in 1:N_years) x',i,'_cov[year, year] += x',i,'nug; \n') #this for loop is just the one line correct? I didn't see any brackets next to it, so I assumed that it was just one line. Line 75 in Noise2.stan
              modstring = paste0(modstring, 'x',i,'_L_cov = cholesky_decompose(x',i,'_cov); \n')
              modstring = paste0(modstring, 'x',i,'_GP_term = x',i,'_L_cov * x',i,'_GP_std;
    }')
            }
            #line 83 below, there is a ' in the code, and I can't tell if its intentional or not. I assumed it was intentional and included it as \'. The original code is line 91 in Noise2.stan
            modstring = paste0(modstring, '{
    matrix[N_years, N_years] y_cov;
    matrix[N_years, N_years] y_L_cov;
    y_cov = sigma_t + years_real*years_real\';
                     for (year in 1:N_years) y_cov[year, year] += ynug;
                     y_L_cov = cholesky_decompose(y_cov);
                     y_GP_term = y_L_cov * y_GP_std;
}
 model { \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring,'vector[N] xz',i,'1; \n')
            }
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'int counter',i,'_in; \n')
              modstring = paste0(modstring, 'int counter',i,'_out; \n')
            }
            
            modstring = paste0(modstring,'int y_counter_in;
  int y_counter_out;
  vector[N] yz; \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'x',i,'nug ~ exponential(1); \n')
            }
            
            modstring = paste0(modstring, 'ynug ~ exponential(1);
  sigma_t ~ gamma(2, .5);
  to_vector(y_GP_std) ~ normal(0, 1); \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'to_vector(x',i,'_GP_std) ~ normal(0, 1); \n')
              modstring = paste0(modstring, 'x',i,'_length_GP_long ~ weibull(30,8); \n')
              modstring = paste0(modstring, 'x',i,'_sigma_GP_long ~ inv_gamma(1,1); \n')
              modstring = paste0(modstring, 'x',i,'_country_sig ~ gamma(2, .5); \n')
              modstring = paste0(modstring, 'x',i,'_year_sig ~ gamma(2, .5); \n')
              modstring = paste0(modstring, 'x',i,'_year_re ~ normal(0, x',i,'_year_sig); \n')
              modstring = paste0(modstring, 'x',i,'_country_re ~ normal(0, x',i,'_country_sig); \n')
              modstring = paste0(modstring, 'x',i,'_sigma ~ gamma(2, .5); \n')
            }
            
            modstring = paste0(modstring, 'y_country_sig ~ gamma(2, .5);
  y_year_sig ~ gamma(2, .5);
  y_year_re ~ normal(0, y_year_sig);
  y_country_re ~ normal(0, y_country_sig); \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'x',i,'b0 ~ normal(0,3); \n')
            }
            modstring = paste0(modstring, 'yb0 ~ normal(0,3);
  B ~ normal(0,3); \n')
            
            for (i in 1:ncov){
              modstring = paste0(modstring, 'counter',i,'_in = 1; \n')
              modstring = paste0(modstring, 'counter',i,'_out = 1; \n')
              modstring = paste0(modstring, 'for(n in 1:N){ \n')
              modstring = paste0(modstring, 'if(counter',i,'_out <= x',i,'_N_miss){ \n')
              modstring = paste0(modstring, 'xz',i,'[n] = x',i,'_miss[counter',i,'_out]; \n')
              modstring = paste0(modstring, 'counter',i,'_out += 1; \n')
              modstring = paste0(modstring, '}else{ \n')
              modstring = paste0(modstring, 'xz',i,'[n] = x',i,'_in[counter',i,'_in]; \n')
              modstring = paste0(modstring, 'counter',i,'_in += 1; \n')
              modstring = paste0(modstring, '} \n')
              modstring = paste0(modstring, '}else{ \n')
              modstring = paste0(modstring, 'xz',i,'[n] = x',i,'_in[counter',i,'_in]; \n')
              modstring = paste0(modstring, 'counter',i,'_in += 1; \n')
              modstring = paste0(modstring, '} \n')
              modstring = paste0(modstring, 'xz',i,'[n] ~ normal(x',i,'b0 + x1_year_re[Year[n]] + x',i,'_country_re[Country[n]] + x',i,'_GP_term[Year[n], Country[n]], x',i,'_sigma); \n')
              modstring = paste0(modstring, '} \n')
              modstring = paste0(modstring, '} \n') #here each of these is going to have an extra } at the end of each bloc, but I think this corresponds to the opening mode {... its in the code you sent, which I imagine works, but just wanted to draw your attention to it (line 166, 185, 202 in Noise2.stan)
            }
            modstring = paste0(modstring, 'y_counter_in = 1;
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
  sig_sq ~ exponential(1);\n')
            for (i in 1:ncov){
              modstring = paste0(modstring, 'b',i,' ~ normal(0,3); \n')
            }
            modstring = paste0(modstring, 'for (n in 1:N) yz[n] ~ normal(yb0 + ')
            for (i in 1:ncov){
              modstring = paste0(modstring, 'xz',i,'[n]*b',i,'1 +')
            }
            modstring = paste0(modstring, 'y_year_re[Year[n]] + y_country_re[Country[n]] +
                       y_GP_term[Year[n], Country[n]], sig_sq);
} \n')
            modstring = paste0(modstring, 'generated quantities {
  vector[y_N_miss] ystar;
  for(nm in 1:y_N_miss) ystar[nm] = normal_rng(y_miss[nm], ', noise,');
  }')
})

 