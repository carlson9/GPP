#runs runMod.R with placebos and given starting noise level and epsilon
#   checks if converged, updates if not
#   once converged, runs it on the actual data, return the Stan fit and noise level
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
                unTUnit = unTUnits[(ql - 1)*nc + nc]
                modText = GPP::writeMod(noise, ncov, printMod)
                d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
                ys = d2[, outcomeName]
                d3 = d2[!(df[, obvColName] == unTUnit & df[, timeColName] > starttime),]
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
                  fit = GPP::runMod(modText, dataBloc)
                  totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                  modLength = max(unique(d3[, timeColName])) - starttime + 1
                  totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                  within[(ql - 1)*cl + cl] = totest > summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                               totest < summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys)))
                
            })
              if(ncores %% nUntreated != 0){
                ncores2 = ncores %% nUntreated
                parrallel::parLapply(cl, 1:ncores2, function(nc) {
                  unTUnit = unTUnits[nUntreated - nc + 1]
                  modText = GPP::writeMod(noise, ncov, printMod)
                  d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
                  ys = d2[, outcomeName]
                  d3 = d2[!(df[, obvColName] == unTUnit & df[, timeColName] > starttime),]
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
                    fit = GPP::runMod(modText, dataBloc)
                    totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                    modLength = max(unique(d3[, timeColName])) - starttime + 1
                    totest = ys[d2[, obvColName] == unTUnit & d2[, timeColName] > starttime]
                    within[(ql - 1)*cl + cl] = totest > summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                      totest < summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys)))
                  
                })
              }
            nn = mean(within) - .95
            if (epsilon < abs(nn)) return(GPP::autoConverge(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores, epsilon, noise = ifelse(nn > 0, noise - shift, noise + shift), printMod, shift))
            return(noise)
})
