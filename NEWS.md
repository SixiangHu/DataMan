DataMan 0.4.1
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
