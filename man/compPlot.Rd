% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compPlot.r
\name{compPlot}
\alias{compPlot}
\title{compPlot: data visualisation comparing observation and model prediction(s).}
\usage{
compPlot(x, act, pred, by = NULL, weights = NULL, exposure = NULL, 
   breaks = NULL, missing=TRUE, newGroupNum = 10,xlim=NULL, 
   xname = "x",yname="y",byname="by",legendPos=NULL)
}
\arguments{
\item{x}{a vector indicates the dependent variable that you want to visulise on (i.e. Age)}

\item{act}{a vector indicates the actual response variable (observation)}

\item{pred}{a vector or data frame that provides model predictions. 
Each column must be from the same model predictions.}

\item{by}{Optinal.A numerical vector to specify the <by> variable}

\item{weights}{Optional. A numerical vector to specify the weights used for calculating weighted average of response.
Normally this is the figures from over/down-samping.}

\item{exposure}{Optional. A numerical vector to specify the exposure used for calculating weighted average of response.}

\item{breaks}{Optional. A vector to specify the breaks for `x`}

\item{missing}{logical. whether to show the `NA` as `Missing` in plot.
If a `Missing` level is already existed, then `NA` will combined}

\item{newGroupNum}{An integer specifies number of new bands 
when levels of current plotting variable `x` or `by` is more than 100.}

\item{xlim}{Optional. A vector provides the range of the variable e.g. xlim=c(0,100).}

\item{xname, yname, byname}{Optional. Characters to be shown on plot.}

\item{legendPos}{Optional. 2 numeric element vector specify the location of legend.}
}
\description{
This function allows you to visualise different model predictions and 
    observations by certain factors.
}
\examples{
 
compPlot(mtcars$vs,
         act=rnorm(nrow(mtcars)),
         pred = data.frame(pred1=rnorm(nrow(mtcars)),
                           pred2=rnorm(nrow(mtcars))),
         by=mtcars$am)
}
\author{
Sixiang.Hu
}
