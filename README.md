# DataMan

[![Build Status](https://travis-ci.org/SixiangHu/DataMan.svg?branch=master)](https://travis-ci.org/SixiangHu/DataMan) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

This is an R package for data cleaning and preliminary data analysis.

### Data Cleaning

Data cleaning have 2 functions at the moment:
* `DetMiss` : This is the function to detecting missing value in a given data frame or vector.
* `PopMiss` : For missing values, we have choices between: deleting, and, populating with mean or mode.
* `quickTable` : Build contingency table from given vectors.

### Preliminary Data Analysis

Currently, there is only one function provided:
* `DataSummary`: Have a better format and more info compared with R core function `summary`.
* `CramersV`: Calculate the Cramers' V statistics for given data set. Useful for independent test.

### Model Access

* `modelPlot`: Used to visualise `glm` and `gbm` model fitted by `ggplot2` and `googleVis` package.
* `sankeyPlot`: (Descoped) Used to visualise `rpart` model fitted by `rpart`.  This has been implemented by `sankeyNetwork` in `networkD3` package, hence descoped from this package.

### Getting Started

You can install `DataMan` from GitHub as follows:

```r
devtools::install_github('SixiangHu/DataMan')
```

### License

This package is free and open source software, licensed under [GPL 2 or later](http://opensource.org/licenses/gpl-license).