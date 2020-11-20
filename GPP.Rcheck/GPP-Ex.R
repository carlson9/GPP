pkgname <- "GPP"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "GPP-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('GPP')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("GPP")
### * GPP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GPP
### Title: Estimates a counterfactual with uncertainty using Gaussian
###   process projection
### Aliases: GPP GPP,ANY-method

### ** Examples


## Not run: 
##D data(GDPdata)
##D out = GPP(df = GDPdata, 
##D     controlVars = c('invest', 'school', 'ind'),
##D     nUntreated = length(unique(GDPdata$country))-1, 
##D     obvColName = 'country', obvname = 'West Germany', 
##D     outcomeName = 'gdp', starttime = 1989, 
##D     timeColName = 'year')
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GPP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotGPPfit")
### * plotGPPfit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotGPPfit
### Title: Plots results of a (converged) model, with true and projected
###   values.
### Aliases: plotGPPfit plotGPPfit,ANY-method

### ** Examples


## Not run: 
##D load(FDIout)
##D load(d)
##D df = d
##D rm(list=d) 
##D 
##D plotGPPfit(fit=fit, df=df, obvColName='country',
##D     obvName='West Germany',outcomeName='gdp', starttime=1990, 
##D     timeColName='year', legendLoc='bottomright', 
##D     xlabel="Test X-Label", ylabel = "Test Y-Label", 
##D     actualdatacol = 'blue', preddatacol = 'green')
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotGPPfit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("writeMod")
### * writeMod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: writeMod
### Title: Writes Stan code for GPP model
### Aliases: writeMod writeMod,ANY-method

### ** Examples

## Not run: 
##D writeMod(noise = 0.25, ncov = 2)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("writeMod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
