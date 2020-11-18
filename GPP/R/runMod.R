#' Runs the model, given the data and treated case (may be a placebo).
#'
#' Returns a fit of the Stan model for all observations. 
#'
#' @param modText This is the string that contains your Stan code. Can be written with \code{\link{writeMod.R}}.
#' @param dataBloc This is the data that you pass to the Stan code. It is automatically generated when you run \code{\link{autoconverge.R}}. 
#' @param iter The number of iterations you would like to run. Defaults to 25,000. 
#' @param filepath Your preferred place to save the fit data. See Details. 
#'
#' @return The fit for the GPP counterfactual Stan model. 
#' @author Devin P. Brown \email{devinpbrown96@@gmail.com} and David Carlson \email{carlson.david@@wustl.edu} 
#' @examples
#'
#' runMod(modText = GPP::writeMod(0.25, 2), dataBloc = , iter = 5, filepath = '~/Desktop/StanFitFiles')
#' 
#' @seealso \code{\link{plotGPPfit.R}} \code{\link{writeMod.R}} \code{\link{GPP.R}} \code{\link{autoconverge.R}}
#' @rdname runMod
#' @aliases runMod,ANY-method
#' @export
setGeneric(name="runMod",
           def=function(modText, dataBloc, obvColName, unit, iter, filepath)
           {standardGeneric("runMod")}
)

#' @export
setMethod(f="runMod",
          definition=function(modText, dataBloc, obvColName, unit, iter = 25000, filepath=NULL){
            require(rstan)
            if (!is.null(filepath)) setwd(filepath)
            fit = rstan::stan(model_code = modText, model_name = unit, data = dataBloc,
                       iter = iter, chains = 1, cores = 1, seed = i,
                       control = list(adapt_delta = .999, max_treedepth = 10, stepsize = 5))
            save(fit, file = paste0(unit, 'Test.Rdata'))
            return(fit)
          })
            
        