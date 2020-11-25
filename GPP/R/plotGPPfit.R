#' Plots results of a (converged) model, with true and projected values.
#'
#' Takes the results of a Gaussian Process Projection fit and generates a linear plot of the actual and predicted counterfactual values
#'
#' @param fit The fit results of the GPP stan model.
#' @param df The dataframe used in your model. 
#' @param obvColName The column name that includes your observation of interest. Must be a string.
#' @param obvName  The name of the specific observation of interest. Must be a string. 
#' @param outcomeName The explanatory variable that is subjected to the counterfactual claim.
#' @param starttime The start time of the treatment effect.
#' @param timeColName The name of the column that includes your time variable. 
#' @param legendLoc The preferred location of the legend in the final graph. Defaults to "topleft". 
#' @param xlabel The label of the x-axis in the final graph. Defaults to input for 'timeColName'.
#' @param ylabel The preferred label of the y-axis in the final graph. Defaults to input for 'outcomeName'.
#' @param actualdatacol The preferred color for plotted line for actual data. Defaults to black. 
#' @param preddatacol The preferred color for plotted line for predicted counterfactual data. Defaults to red. 
#' @param ... Further graphical parameters.
#' 
#' @return A plot built in r-base
#' 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' 
#' @seealso \code{\link{autoConverge}} \code{\link{GPP}} \code{\link{runMod}} \code{\link{writeMod}}
#' @rdname plotGPPfit
#' @aliases plotGPPfit,ANY-method
#' @export
setGeneric(name="plotGPPfit",
           def=function(fit, df, obvColName, obvName, outcomeName, starttime, timeColName, legendLoc = 'topleft', xlabel = NULL, ylabel = NULL, actualdatacol = 'black', preddatacol = 'red', ...)
           {standardGeneric("plotGPPfit")}
)

#' @export
setMethod(f="plotGPPfit",
          definition=function(fit, df, obvColName, obvName, outcomeName, starttime, timeColName, legendLoc = 'topleft', xlabel=NULL, ylabel=NULL, actualdatacol = 'black', preddatacol = 'red',...){
            if (is.null(xlabel)) xlabel = timeColName
            if (is.null(ylabel)) ylabel = outcomeName
            ys = as.numeric(df[,outcomeName])
            ys2 = ys
            ys2[df[,obvColName] == obvName & df[, timeColName] > starttime] = NA
            timelength = 1:(max(df[, timeColName])-starttime+1)
            ys3 = c(ys, summary(fit)$summary[paste0('ystar[', timelength, ']'), '97.5%']*sd(as.numeric(na.omit(ys2))) + mean(as.numeric(na.omit(ys2))), summary(fit)$summary[paste0('ystar[', timelength, ']'), '2.5%']*sd(as.numeric(na.omit(ys2))) + mean(as.numeric(na.omit(ys2))))
            
            
            plot(df[,outcomeName][df[,obvColName]==obvName] ~
                   df[,timeColName][df[,obvColName]==obvName],
                 type = 'l', xlab = xlabel, ylab = paste('(Predicted)',ylabel),
                 ylim = c(min(ys3), max(ys3)), col=actualdatacol, ...)
            lines(starttime:max(df[,timeColName]), summary(fit)$summary[paste0('ystar[', timelength, ']'), 'mean']*sd(as.numeric(na.omit(ys2))) + mean(as.numeric(na.omit(ys2))),
                  type='l', col=preddatacol)
            lines(starttime:max(df[,timeColName]), summary(fit)$summary[paste0('ystar[', timelength, ']'), '2.5%']*sd(as.numeric(na.omit(ys2))) + mean(as.numeric(na.omit(ys2))),
                  type='l', col=preddatacol, lty=2)
            lines(starttime:max(df[,timeColName]), summary(fit)$summary[paste0('ystar[', timelength, ']'), '97.5%']*sd(as.numeric(na.omit(ys2))) + mean(as.numeric(na.omit(ys2))),
                  type='l', col=preddatacol, lty=2)
            legend(legendLoc, legend = c(ylabel, paste('Predicted', ylabel)), col = c(actualdatacol, preddatacol), lty = 1, bty = 'n')
            }
)
          
          