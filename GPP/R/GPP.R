#' Estimates a counterfactual with uncertainty using Gaussian process projection
#'
#' Returns a list of a plot object (after making the plot) of estimated counterfactual values after checking for model convergence and adjusting the noise level, and returns the fitted model.
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
#' @param printMod Boolean. Defaults FALSE. If TRUE, prints each model block to the console. See details.
#' @param shift The magnitude of adjustment for the noise level per iteration. Defaults to 0.05.
#' @param iter The number of iterations you would like to run. Defaults to 25,000. 
#' @param filepath Your preferred place to save the fit data. See Details. 
#' @param legendLoc The preferred location of the legend in the final graph. Defaults to "topleft". 
#' @param xlabel The label of the x-axis in the final graph. Defaults to input for 'timeColName'.
#' @param ylabel The preferred label of the y-axis in the final graph. Defaults to input for 'outcomeName'.
#' @param actualdatacol The preferred color for plotted line for actual data. Defaults to black. 
#' @param predatacol The preferred color for plotted line for predicted counterfactual data. Defaults to red. 
#' 
#' @return A plot of the actual values and the estimated counterfactual values of the model, and the final model fit. 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' @examples
#'
#' \dontrun{
#' 
#' }
#' 
#' @seealso \code{\link{plotGPPfit.R}} \code{\link{writeMod.R}} \code{\link{runMod.R}} \code{\link{autoconverge.R}}
#' @rdname GPP
#' @aliases GPP, ANY-method
#' @export
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
            pp = GPP::plotGPPfit(fit, df, obvColName, obvName, outcomeName, starttime, timeColName, legendLoc, xlabel, ylabel, actualdatacol, preddatacol, ...)
            return(list(pp, fit))
})



