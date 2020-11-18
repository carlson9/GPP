#' Checks Stan model for convergence, then runs model on actual data.
#'
#' Return a converged Stan model fit and the recommended noise level.
#'
#' @param df The dataframe used for the model.
#' @param controlVars String of column names for control variables.
#' @param nUntreated The number of untreated units in the model.
#' @param obvColName The column name that includes the observation subject to the counterfactual.
#' @param obvName The name of the observation subject to the counterfactual. 
#' @param outcomeName The outcome variable of interest. 
#' @param starttime The start year of the counterfactual estimation. 
#' @param timeColName The name of the column that includes the time variable.
#' @param ncores The number of cores to be used to run the model. 
#' @param epsilon The desired level of convergence.
#' @param noise The baseline level of noise to be added to the model to prevent overfit. Updates as the model runs. 
#' @param printMod Boolean. Defaults FALSE. If TRUE, returns the entire Stan model fit. See details.
#' @param shift The magnitude of adjustment for the noise level per iteration. Defaults to 0.05.
#'
#' @return The fit of the converged Stan model and the recommended noise level. 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' @examples
#' \dontrun{
#' 
#' 
#' }
#' 
#' 
#' @seealso \code{\link{plotGPPfit.R}} \code{\link{runMod.R}} \code{\link{GPP.R}} \code{\link{writeMod.R}}
#' @rdname autoConverge
#' @aliases autoConverge,ANY-method
#' @export
setGeneric(name="autoConverge",
           def=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores, epsilon, noise, printMod, shift)
           {standardGeneric("autoConverge")}
)
#' @export
setMethod(f="autoConverge",
          definition=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores = NULL, epsilon = .02, noise = .1, printMod = FALSE, shift = .05){
            unTUnits = unique(df[, obvColName])
            unTUnits = unTUnits[unTUnits != obvName]
            require(parallel)
            if(is.null(ncores)) ncores = parallel::detectCores()
            ncores = min(nUntreated, ncores)
            nLoops = ncores %/% nUntreated
            for(ql in 1:nLoops){
              cl <- parallel::makeCluster(ncores, type='FORK', useXDR = FALSE)
              within = numeric(nUntreated)
              parrallel::parLapply(cl, 1:ncores, function(nc) {
                require(rstan)
                unTUnit = unTUnits[(ql - 1)*nc + nc]
                modText = GPP::writeMod(noise, ncov, printMod)
                d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
                ys = d2[, outcomeName]
                d3 = d2[!(df[, obvColName] == unTUnit & df[, timeColName] > starttime),]
                ys[d2[, obvColName] == obvName & d2[, timeColName] > starttime] = NA
                  xs = list()
                  for(n in 1:length(controlVars)){
                    assign(paste0('xs[[x', n, '_N_obs]]'), sum(!is.na(get(paste0('d3$', controlVars[n])))))
                    assign(paste0('xs[[x', n, '_N_miss]]'), sum(is.na(get(paste0('d3$', controlVars[n])))))
                    assign(paste0('xs[[x', n, '_miss_ind]]'), which(is.na(get(paste0('d3$', controlVars[n])))))
                    assign(paste0('xs[[x', n, 'in]]'), as.numeric(scale(na.omit(get(paste0('d3$', controlVars[n]))))))
                  }
                  dataBloc = c(xs, list(
                    y_N_obs = sum(!is.na(ys)),
                    y_N_miss = sum(is.na(ys)),
                    y_miss_ind = which(is.na(ys)),
                    N_countries = length(unique(d2[, obvColName])),
                    N_years = length(unique(d2[, timeColName])),
                    Country = as.numeric(as.factor(d2[, obvColName])),
                    Year = as.numeric(as.factor(d2[, timeColName])),
                    y_in = as.numeric(scale(as.numeric(na.omit(ys))))
                  ))
                  fit = GPP::runMod(modText, dataBloc, obvColName, unit = unTUnit, iter, filepath)
                  totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                  modLength = max(unique(d3[, timeColName])) - starttime + 1
                  totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                  within[(ql - 1)*cl + cl] = totest > rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                               totest < rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys)))
                
            })
              if(ncores %% nUntreated != 0){
                ncores2 = ncores %% nUntreated
                parrallel::parLapply(cl, 1:ncores2, function(nc) {
                  require(rstan)
                  unTUnit = unTUnits[nUntreated - nc + 1]
                  modText = GPP::writeMod(noise, ncov, printMod)
                  d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
                  ys = d2[, outcomeName]
                  d3 = d2[!(df[, obvColName] == unTUnit & df[, timeColName] > starttime),]
                  ys[d2[, obvColName] == obvName & d2[, timeColName] > starttime] = NA
                    xs = list()
                    for(n in 1:length(controlVars)){
                      assign(paste0('xs[[x', n, '_N_obs]]'), sum(!is.na(get(paste0('d3$', controlVars[n])))))
                      assign(paste0('xs[[x', n, '_N_miss]]'), sum(is.na(get(paste0('d3$', controlVars[n])))))
                      assign(paste0('xs[[x', n, '_miss_ind]]'), which(is.na(get(paste0('d3$', controlVars[n])))))
                      assign(paste0('xs[[x', n, 'in]]'), as.numeric(scale(na.omit(get(paste0('d3$', controlVars[n]))))))
                    }
                    dataBloc = c(xs, list(
                      y_N_obs = sum(!is.na(ys)),
                      y_N_miss = sum(is.na(ys)),
                      y_miss_ind = which(is.na(ys)),
                      N_countries = length(unique(d2[, obvColName])),
                      N_years = length(unique(d2[, timeColName])),
                      Country = as.numeric(as.factor(d2[, obvColName])),
                      Year = as.numeric(as.factor(d2[, timeColName])),
                      y_in = as.numeric(scale(as.numeric(na.omit(ys))))
                    ))
                    fit = GPP::runMod(modText, dataBloc, obvColName, unit = unTUnit, iter, filepath)
                    totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                    modLength = max(unique(d3[, timeColName])) - starttime + 1
                    totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                    within[(ql - 1)*cl + cl] = totest > rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                      totest < rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys)))
                  
                })
              }
            nn = mean(within) - .95
            if (epsilon < abs(nn)) return(GPP::autoConverge(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores, epsilon, noise = ifelse(nn > 0, noise - shift, noise + shift), printMod, shift))
            return(noise)
})
