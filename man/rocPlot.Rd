% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rocPlot.r
\name{rocPlot}
\alias{rocPlot}
\title{ROC (AUC) plot}
\usage{
rocPlot(data,actual)
}
\arguments{
\item{data}{A numeric vector, `martix`, `data.table` or `data.frame` that each column stands for predictions from a model.}

\item{actual}{A numeric vector for actual or true results.}
}
\description{
With provided different model predictions, different lift curves will be 
plot to compare model improvement.
}
\examples{
set.seed(1L)
a <- sample(c(0,1),10000,replace=TRUE)
b <- runif(10000)
c <- runif(10000)
rocPlot(cbind(b,c),a)
}
\author{
Sixiang Hu
}
