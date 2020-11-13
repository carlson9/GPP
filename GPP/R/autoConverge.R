#runs runMod.R with placebos and given starting noise level and epsilon
#   checks if converged, updates if not
#   once converged, runs it on the actual data, return the Stan fit and noise level
setGeneric(name="autoConverge",
           def=function(df, nUntreated, outcomeName, obvColName, obvName, expv, starttime, timeColName, ncov, epsilon, noise, printMod, shift)
           {standardGeneric("autoConverge")}
)
#' @export
setMethod(f="autoConverge",
          definition=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, epsilon = .02, noise = .1, printMod = FALSE, shift = .05){
            modText = GPP::writeMod(noise, ncov, printMod)
            d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
            ys = d2[, outcomeName]
            within = c()
            for(i in 1:nUntreated){
              xs = list()
              for(n in 1:length(controlVars)){
                assign(paste0('xs[[x', n, '_N_obs]]'), sum(!is.na(get(paste0('df$', controlVars[n])))))
                assign(paste0('xs[[x', n, '_N_miss]]'), sum(is.na(get(paste0('df$', controlVars[n])))))
                assign(paste0('xs[[x', n, '_miss_ind]]'), which(is.na(get(paste0('df$', controlVars[n])))))
              }
            dataBloc = c(xs, list(
                y_N_obs = sum(!is.na(ys)),
                y_N_miss = sum(is.na(ys)),
                y_miss_ind = which(is.na(ys)),
                N_countries = length(unique(d$country)),
                N_years = length(unique(d$year)),
                Country = as.numeric(as.factor(d$country)),
                Year = as.numeric(as.factor(d$year)),
                x1_in = as.numeric(scale(na.omit(xs1))),
                x2_in = as.numeric(scale(na.omit(xs2))),
                y_in = as.numeric(scale(as.numeric(na.omit(ys))))
              ))
            
              unique(df[df[, obvColName != obvName], obvColName])[i]
              mod = GPP::runMod(modText, dataBloc)
              totest = ys[df[, obvColName] == df[, obvColName][i] & df[, timeColName] > starttime]
              modLength = max(unique(df[, timeColName])) - starttime
              totest = ys[df[, obvColName] == df[, obvColname][i] & df[, timeColName] > starttime]
              within = c(within, totest > summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                           totest < summary(fit)$summary[paste0('ystar[', 1:modLength, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))))
            }
            nn = mean(within) - .95
            if (epsilon < abs(nn)) return(GPP::autoConverge(df, nUntreated, outcomeName, obvColName, obvName, expv, starttime, timeColName, ncov, epsilon, noise = ifelse(nn > 0, noise - shift, noise + shift), printMod))
            return(noise)
})