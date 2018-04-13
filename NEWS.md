DataMan 0.5.3
-------------------------------------------------------------
NEW FEATURES
* Now all the plots uses `plotly` package.

DataMan 0.5.2
-------------------------------------------------------------

BUG FIXES AND MINOR IMPROVEMENTS
* fix issue when `byvar` is numeric orinteger format, the histogram is not showing properly.

DataMan 0.5.1
-------------------------------------------------------------
NEW FEATURES
* compPlot to compare different model predictions in a plot.

BUG FIXES AND MINOR IMPROVEMENTS
* The axis title of 3d plot can now be set correctly with latest version of `plotly` on github. (#31)


DataMan 0.5
-------------------------------------------------------------
NEW FEATURES
* add dmBreak function to give pretty groups

* interPlot function will automatically group x or y if there are too many levels

* utilising `plotly` package for graphics, instead of `rbokeh`

CHANGES
* modelPlot function has been rewritten which has been generised.

* `base` parameter in `ModelData` has been depricated as this is not practical to
change the base for all models.

* dataComp function has been rewritten to provide a varComp function.

DataMan 0.4.3
-------------------------------------------------------------
NEW FEATURES

* add modelMetric function to conduct statistical analysis on predictions.

* add dataComp function to check whether the factors in a new dataset is consistent as before.

* add multiParts function to split a vector into different parts by provided proportion.

CHANGES
* interPlot now support weights parameter. 

* modelPlot can specify the base of a factor when viewing a glm model.

DataMan 0.4.2
-------------------------------------------------------------
NEW FEATURES

* Add `interPlot` that gives 3D surface plot utilising `plotly` package.

* Add `liftPlot` that compares different model predictions.

* Add `rocPlot` that assesses binary classification under `ROCR` package. It plots ROC curves with one or more predictions. AUC will be calculated and showed in legend.

* Change the visual package from `ggplot2` to `rbokeh`.  The new package gives interactive experience like zooming, hover, and linked data for different plots.

BUG FIXES AND MINOR IMPROVEMENTS

* To pass the `R CMD check` on 0 warnings and 0 notes.

* Move misc functions to `utils.r` function

* Change the glyphs of points in `modelPlot`

* Change `.travis.yaml` to install `rbokeh` from github.

* Improve the `PopMiss`.
