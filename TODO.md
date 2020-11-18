# TODO (update as we go) #
* write a function that checks coverage of placebos - #have we done this?

* Check documentation for autoConverge and GPP. I'm not sure whether GPP as the wrapper function needs different documentation than a sub-function. 
* Check brackets/ parentheses in autoConverge. RStudio is showing an error.
* Right now it looks like autoConverge is just returning noise level, so we will need to go back and either have runMod create the dataBloc to feed into the Stan model, or we need to have autoconverge return the printed dataBloc for runMod

* (Together) Write example for autoConverge, runMod (dataBloc), and GPP 
* Add details section for autoConverge @param printMod & ncores. Details runMod @param filepath
