% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelMetric.R
\name{modelMetric}
\alias{modelMetric}
\title{modelMetric}
\usage{
modelMetric(act,pred)
}
\arguments{
\item{act}{numeric vector of actual data}

\item{pred}{numeric vector or data frame. Should have the same length as `act`. 
It can also be a data frame that compare different model predictions at the same time.}
}
\description{
This function compares model prediction(s) with actual measured by different statistics.
}
\details{
Currently, it conducts MSE, RMSE, MAE, RMAE, and LogLoss (for clasification) analysis.
}
\examples{

actual <- rnorm(1000)
y <- rnorm(1000)
z <- rnorm(1000)
pred <- data.frame(model1=y,model2=z)
modelMetric(actual,pred)
}
\author{
Sixiang Hu
}
