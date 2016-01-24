# DataMan

[![Build Status](https://travis-ci.org/SixiangHu/DataMan.svg?branch=master)](https://travis-ci.org/SixiangHu/DataMan) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

This is an R package for data cleaning, preliminary data analysis and modeling assessing.

### Data Cleaning

Data cleaning have 3 functions at the moment:

* `DetMiss` : This is the function to detecting missing value in a given data frame, data.table or vector.

* `PopMiss` : For missing values, we have choices in: deleting, replace, or populating with mean or mode.

### Preliminary Data Analysis

* `DataSummary`: Have a better format and more info compared with R function `summary` in `base`.

* `CramersV`: Calculate the Cramers' V statistics on a given data set. Useful for independent test.

* `dataPlot`: Visualise data feature.

* `dataComp`: Compare two dataset to check whether there is a profile change.

### Model Assess

* `modelPlot`: visualise model fitting effect.

```{r}
glm1 <- glm(formula = mpg ~ cyl + hp, family = Gamma(log), data = mtcars, weights = wt)
modelPlot(glm1,"cyl","mpg","wt",dataset=mtcars,type="response")
```

* `liftPlot`: visualising different model predictiveness by plotting lift curves on the same graph.

* `resiPlot`: assessing the residual using given actual and predicted values.

* `rocPlot`: comparing binary model predictions in roc curve (AUC).

* `interPlot`: visualising data feature or model predictions on a 3D interactive plot.

* `modelMetric`: Gives simple comparision for one or a set of models.

* `tree2data`: function used to collect information from `gbm` or `randomForest` model object to create data for `sankeyNetwork` plot in `networkD3` package.

```{r}
library(networkD3)
data(iris)
iris.mod <- gbm(Species ~ ., distribution="multinomial", data=iris, n.trees=2000, shrinkage=0.01, cv.folds=5, verbose=FALSE, n.cores=1)
tree_data <- tree2data(iris.mod,1)
sankeyNetwork(tree_data[[1]],tree_data[[2]],Source="src",Target="tar",Value="value",NodeID="name")
```

### Getting Started

You can install `DataMan` from GitHub as follows:

```r
devtools::install_github("bokeh/rbokeh")

devtools::install_github('SixiangHu/DataMan')
```

### License

This package is free and open source software, licensed under [GPL 2 or later](http://opensource.org/licenses/gpl-license).
