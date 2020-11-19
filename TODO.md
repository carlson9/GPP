# TODO (update as we go) #
* write a function that checks coverage of placebos - #have we done this?

* Check documentation for autoConverge, GPP, and runMod. I'm not sure whether GPP as the wrapper function needs different documentation than a sub-function. 
* Check brackets/ parentheses in autoConverge. RStudio is showing an error.
* Right now it looks like autoConverge is just returning noise level, so we will need to go back and either have runMod create the dataBloc to feed into the Stan model, or we need to have autoconverge return the printed dataBloc for runMod ( I might be wrong here but we can talk about it)
* Do we also need to adjust the iterations to make sure that the people's model is converging? Would it be a good idea (maybe for a future update) to find the optimal iter # so that the models take the shortest time possible while still converging?

* (Together) Write example for autoConverge, runMod (dataBloc), and GPP 

