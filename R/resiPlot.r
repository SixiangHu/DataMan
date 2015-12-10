#' resiPlot
#'
#' @description This function assess the residual using given actual and predicted values.
#' @usage resiPlot(act,pred,weight=NULL,bucket=20)
#' @param act numerical vector for actual observation.
#' @param pred numerical vector for model preidctions.  It must have the same length as act.
#' @param weight numerical vector to give weight to observations.
#' @param bucket Integer. It specifies the number of bucket of the AvsE plot.
#' @details 
#' Currently, the residual in this function is defined as: Residual = actual - predicted.  I don't use `resi` or `reisdual` function
#' from `stats` package because this function will be used for much wider model assess (e.g. `randomFoest`, `gbm`), and 2 functions
#' mentioned above can only applied to `glm` and `lm`.
#'
#' This function will give 2 plots:
#' AvsE plot: The aggregated (by provided number of bucket) average actual and predicted value, with a diagnal line for comparison.
#' Residual vs Prediction: This plot is used to assess the baise and heterogeneity
#' 
#' @author Sixiang Hu
#' @importFrom dplyr left_join group_by summarise
#' @importFrom rbokeh ly_abline ly_points ly_hexbin grid_plot
#' @export resiPlot
#' @examples
#' act <- rnorm(10000)
#' pred <- rgamma(10000,1)
#' resiPlot(act,pred)

resiPlot <- function(act,pred,weight=NULL,bucket=20){
  rng <- range(rbind(act,pred),na.rm = TRUE,finite=TRUE)
  
  if (is.na(rng) || is.infinite(rng)) stop("Given data is all NA's or infinite's.")
  
  by <- ( rng[2] -rng[1] ) / bucket
  
  label <- seq(rng[1] + by/2,rng[2] - by/2,by=by)
  
  act_cuts <- cut(pred,breaks=seq(rng[1],rng[2],by=by),
              ordered_result = TRUE,include.lowest = TRUE)
  
  label_cuts <- cuts <- cut(label,breaks=seq(rng[1],rng[2],by=by),
                            ordered_result = TRUE,include.lowest = TRUE)
  
  if (length(act) != length(weight) && length(weight) >0 ){
    weight <- NULL
    warning("Given weight has different length as actual value, will be ingored.")
  }
    
  if (is.null(weight)) weight <- rep(1,length(act))

  temp <- data.table::as.data.table(cbind(act,cuts=act_cuts,weight))
  temp_lab <- data.table::as.data.table(cbind(label,cuts=label_cuts))
    
  temp_act <- temp %>% 
    dplyr::group_by(cuts) %>% 
    dplyr::summarise(x = weighted.mean(act, weight))

  df_act <- dplyr::left_join(temp_lab,temp_act,by= "cuts")
      
  #AvsE plot
  AvsE <- rbokeh::figure(tools=.tools,xlab="Predicted",ylab="Actual",height=500) %>%
    rbokeh::ly_points(label,x,data=df_act,color="blue",size=10,
                      hover="<strong>Actual: </strong>@x<br><strong>Predict: </strong>@label") %>%
    rbokeh::ly_abline(a=0,b=1,size=1,color="red")

  #residual
  res <- data.frame(res=act - pred,pred=pred)
  
  Resi <- figure(xlab="Predicted",ylab="Residuals (Actual - Expected)",height=250) %>% 
    ly_hexbin(pred,res,data=res)  
  
  grid_plot(list(AvsE,Resi),nrow=2,ncol=1,width=900,same_axes = c(TRUE,FALSE))
}

#global variable
globalVariables(c("x"))
