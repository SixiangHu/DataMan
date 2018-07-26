#' modelMetric
#' 
#' @description This function compares model prediction(s) with actual measured by different statistics.
#' @usage modelMetric(act,pred)
#' @param act numeric vector of actual data
#' @param pred numeric vector or data frame. Should have the same length as `act`. 
#' It can also be a data frame that compare different model predictions at the same time.
#' @details Currently, it conducts MSE, RMSE, MAE, RMAE, and LogLoss (for clasification) analysis.
#' 
#' @author Sixiang Hu
#' 
#' @importFrom data.table data.table
#' 
#' @export
#' 
#' @examples
#' 
#' actual <- rnorm(1000)
#' y <- rnorm(1000)
#' z <- rnorm(1000)
#' pred <- data.frame(model1=y,model2=z)
#' modelMetric(actual,pred)

modelMetric <- function(act,pred){
  #Mean square error
  mse <- function(pred,act){
    mean( (pred-act)^2 , na.rm = TRUE )
  }
  
  #Mean absolute error
  mae <- function(pred,act){
    mean(abs(act-pred), na.rm = TRUE )
  }

  MM_mse <- as.numeric(as.matrix(unlist(lapply(pred,mse,act=act))))
  MM_mae <- as.numeric(as.matrix(unlist(lapply(pred,mae,act=act))))
  
  #logloss
  
  #mlogloss
  
  res <- data.table(MSE = MM_mse,RMSE = sqrt(MM_mse),
                    MAE = MM_mae,RMAE = sqrt(MM_mae))
  rownames(res) <- names(pred)
  t(res)
}


#' residual deviance
#' 
#' @usage residualDeviance(act,pred,family,p=NULL,wt=NULL)
#' 
#' @description weighted residual deviance
#'
#' @param act Numeric. Actual
#' @param pred Numeric. Predicted or fitted.
#' @param family Character. One of "poisson","gamma","normal","tweedie","normal","wald",or "inverse.gaussian".
#' @param p variance function power when `family` = "tweedie"
#' @param wt Numeric vector. Weight of each records.
#'
#' @return residual deviance numeric vector
#' @export
#'
#' @examples
#' 
#' act = rpois(100,1)
#' pred = act + abs(rnorm(100))
#' residualDeviance(act,pred,family = "poisson")
#' 
residualDeviance <- function(act,pred,family,p=NULL,wt=NULL){
  if(is.null(wt)) wt = rep(1,length(act))
  
  if(family == "tweedie" & !is.null(p)){
    if(p==0) family = "normal"
    if(p==1) family = "poisson"
    if(p==2) family = "gamma"
    if(p==3) family = "inverse.gaussian"
  }else if(family == "tweedie" & is.null(p)){
    stop("p value is not provided when family is tweedie.")
  }
  
  r = switch(family,
             normal  = -(act - pred)^2 / 2,
             poisson = ifelse(act>0,((act - pred) - act * log(act/pred)),-pred),
             gamma   = log(ifelse(act ==0, 1, act/pred)) - (act - pred)/pred,
             tweedie = act*pred^(1 - p)/(1 - p) - act^(2 - p)/((1 - p)*(2 - p)) - pred^(2 - p)/(2 - p),
             wald = ,
             inverse.gaussian = (-(act - pred)^2)/(act*pred^2)/2,
             0
             )
  
  return(-2*r*wt)
}