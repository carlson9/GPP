#runs runMod.R with placebos and given starting noise level and epsilon
#   checks if converged, updates if not
#   once converged, runs it on the actual data, return the Stan fit and noise level
setGeneric(name="autoConverge",
           def=function(df, nUntreated, outcomeName, obvColName, obvName, expv, starttime, timeColName, ncov, epsilon, noise, printMod)
           {standardGeneric("autoConverge")}
)
#' @export
setMethod(f="autoConverge",
          definition=function(df, nUntreated, outcomeName, obvColName, obvName, expv, starttime, timeColName, ncov, epsilon = .02, noise = .1, printMod = FALSE){
            modText = GPP::writeMod(noise, ncov, printMod)
            for(i in 1:nUntreated){
              mod = GPP::runMod(modText, df, unique(obvColName)[i])
              d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
              ys = d2[, outcomeName]
              totest = ys[d2[, obvColName] == df[, obvColName][i] & d2[, timeColName] > starttime]
              
              within = c(within, totest > summary(fit)$summary[paste0('ystar[', 1:14, ']'), '2.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))) &
                           totest < summary(fit)$summary[paste0('ystar[', 1:14, ']'), '97.5%']*sd(as.numeric(na.omit(ys))) + mean(as.numeric(na.omit(ys))))
            }
            
            while (epsilon > nn) {
              
            }
})