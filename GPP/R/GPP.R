#wrapper function for GPP analysis
setGeneric(name="autoConverge",
           def=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores, epsilon, noise, printMod, shift, legendLoc, xlabel, ylabel, actualdatacol, preddatacol,...)
           {standardGeneric("GPP")}
)
#' @export
setMethod(f="GPP",
          definition=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores = NULL, epsilon = .02, noise = .1, printMod = FALSE, shift = .05, iter = 25000, filepath = NULL, legendLoc = 'topleft', xlabel=NULL, ylabel=NULL, actualdatacol = 'black', preddatacol = 'red',...){
            noise = GPP::autoConverge(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores, epsilon, noise, printMod, shift)
            
            modText = GPP::writeMod(noise, length(controlVars), printMod)
            
            d2 = df[!(df[, obvColName] == obvName & df[, timeColName] > starttime),]
            ys = df[, outcomeName]
            ys[df[, obvColName] == obvName & df[, timeColName] > starttime] = NA
            xs = list()
            for(n in 1:length(controlVars)){
              assign(paste0('xs[[x', n, '_N_obs]]'), sum(!is.na(get(paste0('d2$', controlVars[n])))))
              assign(paste0('xs[[x', n, '_N_miss]]'), sum(is.na(get(paste0('d2$', controlVars[n])))))
              assign(paste0('xs[[x', n, '_miss_ind]]'), which(is.na(get(paste0('d2$', controlVars[n])))))
              assign(paste0('xs[[x', n, 'in]]'), as.numeric(scale(na.omit(get(paste0('d2$', controlVars[n]))))))
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
            
            fit = GPP::runMod(modText, dataBloc, obvColName, unit = obvName, iter, filepath)
            return(GPP::plotGPPfit(fit, df, obvColName, obvName, outcomeName, starttime, timeColName, legendLoc, xlabel, ylabel, actualdatacol, preddatacol, ...))
})



