#' Runs the model, given the data and treated case (may be a placebo).
#'
#' Returns a fit of the Stan model for all observations. 
#' 
#' @details 
#' 
#' For iterations, check that your model converged (we recommend all r-hats close to 1 and examining traceplots).
#' 
#' We recommend creating a new folder for the file path since the Stan fit creates a large number of files at runtime.
#'
#' @param modText This is the string that contains your Stan code. Can be written with \code{\link{writeMod}}.
#' @param dataBloc This is the data that you pass to the Stan code. It is automatically generated when you run \code{\link{autoConverge}}.
#' @param unit The unit of observation to project.  
#' @param iter The number of iterations you would like to run. Defaults to 25,000. 
#' @param filepath Your preferred place to save the fit data. See Details. 
#'
#' @return The fit for the GPP counterfactual Stan model. 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' 
#' @seealso \code{\link{plotGPPfit}} \code{\link{writeMod}} \code{\link{GPP}} \code{\link{autoConverge}}
#' @rdname runMod
#' @aliases runMod,ANY-method
#' @export
setGeneric(name="runMod",
           def=function(modText, dataBloc, unit, iter = 25000, filepath = NULL)
           {standardGeneric("runMod")}
)

#' @export
setMethod(f="runMod",
          definition=function(modText, dataBloc, unit, iter = 25000, filepath=NULL){
            if (!is.null(filepath)) setwd(filepath)
            fit = rstan::stan(model_code = modText, model_name = unit, data = dataBloc,
                       iter = iter, chains = 1, cores = 1,
                       control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
            save(fit, file = paste0(unit, 'Test.Rdata'))
            return(fit)
          })
            
        