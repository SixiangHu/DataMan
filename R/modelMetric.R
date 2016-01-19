#' modelMetric
#' 
#' @description This function compares model prediction(s) with actual measured by different statistics.
#' @usage modelMetric(act,pred)
#' @param act numeric vector of actual data
#' @param pred numeric vector or data frame. Should have the same length as `act`. 
#' It can also be a data frame that compare different model predictions at the same time.
#' @details Currently, it conducts MSE, RMSE, MAE, RMAE, and LogLoss (for clasification) analysis.
#' @author Sixiang Hu
#' @export modelMetric
#' @examples
#' 
#' x <- rnorm(1000)
#' y <- rnorm(1000)
#' z <- rnorm(1000)
#' pred <- data.frame(model1=y,model2=z)
#' modelMetric(x,pred)

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
  
  res <- data.frame(MSE = MM_mse,RMSE = sqrt(MM_mse),
                    MAE = MM_mae,RMAE = sqrt(MM_mae))
  rownames(res) <- names(pred)
  t(res)
}