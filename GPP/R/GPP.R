#wrapper function for GPP analysis

#needs to take the outcome variable name, control name(s), dataset name,
#   epsilon for noise tolerance, test case name, case variable name,
#   time variable name, indicator for treatment time,
#   noise starting level (default .1),
#   specific graphical parameters, and ... for additional graphical parameters

#needs to write the model with the number of covariates and noise level
#then needs to check placebos, adjust the noise (re-write the model)
#update until convergence
#then run on the test case
#then plot the results
