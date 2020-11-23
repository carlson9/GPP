#' Checks Stan model for convergence, then runs model on actual data.
#'
#' Return a converged Stan model fit and the recommended noise level.
#' 
#' @details 
#' We recommend creating a new folder for the file path since the Stan fit creates a large number of files at runtime.
#' 
#' For iterations, check that your model converged (we recommend all r-hats close to 1 and examining traceplots).
#' 
#' We recommend keeping printMod as FALSE, otherwise, the function will write the model to the console for every model run on the convergence.
#' 
#' We also recommend using all cores on your machine to speed up model run time. If you are unsure about the number of cores in your machine, see doParallel::detectCores().
#'
#' @param df The dataframe used for the model.
#' @param controlVars String of column names for control variables.
#' @param nUntreated The number of untreated units in the model.
#' @param obvColName The column name that includes the observation subject to the counterfactual.
#' @param obvName The name of the observation subject to the counterfactual. 
#' @param outcomeName The outcome variable of interest. 
#' @param starttime The start time of the counterfactual estimation. 
#' @param timeColName The name of the column that includes the time variable.
#' @param filepath Your preferred place to save the fit data. See Details.
#' @param ncores The number of cores to be used to run the model. Default of NULL will utilize all cores.
#' @param iter Preferred number of iterations. See details. 
#' @param epsilon The desired level of convergence, i.e. how close to the 0.95 coverage is acceptable.
#' @param noise The baseline level of noise to be added to the model to prevent overfit. Updates as the model runs. 
#' @param printMod Boolean. Defaults FALSE. If TRUE, prints the model block for the run to the console. See details.
#' @param shift The magnitude of adjustment for the noise level per iteration. Defaults to 0.05.
#'
#' @return The recommended noise level after convergence.
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' 
#' @seealso \code{\link{plotGPPfit}} \code{\link{runMod}} \code{\link{GPP}} \code{\link{writeMod}}
#' @rdname autoConverge
#' @aliases autoConverge,ANY-method
#' @export
setGeneric(name="autoConverge",
           def=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, filepath = NULL, ncores = NULL, iter = 25000, epsilon = .02, noise = .1, printMod = FALSE, shift = .05)
           {standardGeneric("autoConverge")}
)

#' @export
setMethod(f="autoConverge",
          definition=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, filepath = NULL, ncores = NULL, iter = 25000, epsilon = .02, noise = .1, printMod = FALSE, shift = .05){
            unTUnits = unique(df[, obvColName])
            unTUnits = unTUnits[unTUnits != obvName]
            if(is.null(ncores)) ncores = parallel::detectCores()
            ncores = min(nUntreated, ncores)
            nLoops = nUntreated %/% ncores
            if(nLoops == 0) nLoops = 1
            for(ql in 1:nLoops){
              cl <- parallel::makeCluster(ncores, type='FORK', useXDR = FALSE)
              within = list()
              parallel::parLapply(cl, 1:ncores, function(nc) {
                unTUnit = unTUnits[(ql - 1)*nc + nc]
                modText = GPP::writeMod(noise, ncov = length(controlVars), printMod)
                d2 = df[!(df[, obvColName] == obvName & df[, timeColName] >= starttime),]
                d2[d2[, obvColName] == unTUnit & d2[, timeColName] >= starttime, c(outcomeName, controlVars)] = NA
                ys = d2[, outcomeName]
                  xs = list()
                  for(n in 1:length(controlVars)){
                    xs[[paste0('x', n, '_N_obs')]] = sum(!is.na(d2[, controlVars[n]]))
                    xs[[paste0('x', n, '_N_miss')]] = sum(is.na(d2[, controlVars[n]]))
                    xs[[paste0('x', n, '_miss_ind')]] = which(is.na(d2[, controlVars[n]]))
                    xs[[paste0('x', n, '_in')]] = as.numeric(scale(na.omit(d2[, controlVars[n]])))
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
                  fit = GPP::runMod(modText, dataBloc, unit = unTUnit, iter, filepath)
                  totest = df[df[, obvColName] == unTUnit & df[, timeColName] >= starttime, outcomeName]
                  modLength = max(unique(d2[, timeColName])) - starttime + 1
                  within[[(ql - 1)*nc + nc]] = totest > rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                               totest < rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys)))
                  
            })}
              if(nUntreated %% ncores != 0 & nUntreated > ncores){
                ncores2 = nUntreated %% ncores
                parallel::parLapply(cl, 1:ncores2, function(nc) {
                  unTUnit = unTUnits[nUntreated - nc + 1]
                  modText = GPP::writeMod(noise, ncov = length(controlVars), printMod)
                  d2 = df[!(df[, obvColName] == obvName & df[, timeColName] >= starttime),]
                  d2[d2[, obvColName] == unTUnit & d2[, timeColName] >= starttime, c(outcomeName, controlVars)] = NA
                  ys = d2[, outcomeName]
                  xs = list()
                  for(n in 1:length(controlVars)){
                    xs[[paste0('x', n, '_N_obs')]] = sum(!is.na(d2[, controlVars[n]]))
                    xs[[paste0('x', n, '_N_miss')]] = sum(is.na(d2[, controlVars[n]]))
                    xs[[paste0('x', n, '_miss_ind')]] = which(is.na(d2[, controlVars[n]]))
                    xs[[paste0('x', n, '_in')]] = as.numeric(scale(na.omit(d2[, controlVars[n]])))
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
                    fit = GPP::runMod(modText, dataBloc, unit = unTUnit, iter, filepath)
                    totest = df[df[, obvColName] == unTUnit & df[, timeColName] >= starttime]
                    modLength = max(unique(d2[, timeColName])) - starttime + 1
                    within[[nUntreated - nc + 1]] = totest > rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                      totest < rstan::summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys)))
                  
                })
              }
            nn = mean(unlist(within)) - .95
            if (epsilon < abs(nn)){
              noise = ifelse(nn > 0, noise - shift, noise + shift)
              return(GPP::autoConverge(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores, epsilon, noise, printMod, shift))
            }
            return(noise)
})
