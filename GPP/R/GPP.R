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
#' @param ncores The number of cores to be used to run the model. See details.
#' @param epsilon The desired level of convergence.
#' @param noise The baseline level of noise to be added to the model to prevent overfit. Updates as the model runs. 
#' @param printMod Boolean. Defaults FALSE. If TRUE, prints each model block to the console. See details.
#' @param shift The magnitude of adjustment for the noise level per iteration. Defaults to 0.05.
#' @param iter The number of iterations you would like to run. Defaults to 25,000. See details.
#' @param filepath Your preferred place to save the fit data. See Details. 
#' @param legendLoc The preferred location of the legend in the final graph. Defaults to "topleft". 
#' @param xlabel The label of the x-axis in the final graph. Defaults to input for 'timeColName'.
#' @param ylabel The preferred label of the y-axis in the final graph. Defaults to input for 'outcomeName'.
#' @param actualdatacol The preferred color for plotted line for actual data. Defaults to black. 
#' @param preddatacol The preferred color for plotted line for predicted counterfactual data. Defaults to red. 
#' @param ... Further parameters passed to the plot function.
#' 
#' @details 
#' We recommend using all cores on your machine to speed up model run time. If you are unsure about the number of cores in your machine, see \code{parallel::detectCores()}.
#' 
#' We recommend keeping printMod as FALSE, otherwise, the function will write the model to the console for every model run on the convergence.
#' 
#' For iterations, check that your model converged (we recommend all r-hats close to 1 and examining traceplots).
#' 
#' We recommend creating a new folder for the file path since the Stan fit creates a large number of files at runtime.
#' 
#' 
#' @return A plot of the actual values and the estimated counterfactual values of the model, and the final model fit. 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' @examples
#'
#' \donttest{
#' data(GDPdata)
#' out = GPP(df = GDPdata, 
#'     controlVars = c('invest', 'school', 'ind'),
#'     nUntreated = length(unique(GDPdata$country))-1, 
#'     obvColName = 'country', obvName = 'West Germany', 
#'     outcomeName = 'gdp', starttime = 1989, 
#'     timeColName = 'year',
#'     ncores = 2)
#' }
#' 
#' @seealso \code{\link{plotGPPfit}} \code{\link{writeMod}} \code{\link{runMod}} \code{\link{autoConverge}}
#' @rdname GPP
#' @aliases GPP,ANY-method
#' @export
setGeneric(name="GPP",
           def=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores = NULL, epsilon = .02, noise = .1, printMod = FALSE, shift = .05, iter = 25000, filepath = NULL, legendLoc = 'topleft', xlabel=NULL, ylabel=NULL, actualdatacol = 'black', preddatacol = 'red',...)
           {standardGeneric("GPP")}
)
#' @export
setMethod(f="GPP",
          definition=function(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, ncores = NULL, epsilon = .02, noise = .1, printMod = FALSE, shift = .05, iter = 25000, filepath = NULL, legendLoc = 'topleft', xlabel=NULL, ylabel=NULL, actualdatacol = 'black', preddatacol = 'red',...){
            noise = GPP::autoConverge(df, controlVars, nUntreated, obvColName, obvName, outcomeName, starttime, timeColName, filepath, ncores, iter, epsilon, noise, printMod, shift)

            modText = GPP::writeMod(noise, length(controlVars), printMod)
            
            d2 = df
            d2[d2[, obvColName] == obvName & d2[, timeColName] > starttime, c(outcomeName, controlVars)] = NA
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
            
            fit = GPP::runMod(modText, dataBloc, unit = obvName, iter, filepath)
            pp = GPP::plotGPPfit(fit, df, obvColName, obvName, outcomeName, starttime, timeColName, legendLoc, xlabel, ylabel, actualdatacol, preddatacol, ...)
            return(list(pp, fit))
})



