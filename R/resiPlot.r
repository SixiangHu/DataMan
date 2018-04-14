#' resiPlot
#' 
#' @description This function assess the residual using given actual and
#'   predicted values.
#' @usage resiPlot(act,pred,weight=NULL,exposure=NULL,bucket=20)
#' @param act numerical vector for actual observation.
#' @param pred numerical vector for model preidctions.  It must have the same
#'   length as act.
#' @param weight numerical vector to give weight to observations.
#' @param exposure numerical vector to give exposure to observations.
#' @param bucket Integer. It specifies the number of bucket of the AvsE plot.
#' @details Currently, the residual in this function is defined as: Residual =
#' actual - predicted.  `resi` or `reisdual` function from `stats`
#' package are not used because this function will be used for much wider model assess (e.g.
#' `randomFoest`, `gbm`), and 2 functions mentioned above can only applied to
#' `glm` and `lm`.
#' 
#' This function will give 2 plots: AvsE plot: The aggregated (by provided
#' number of bucket) average actual and predicted value, with a diagnal line for
#' comparison. Residual vs Prediction: This plot is used to assess the baise and
#' heterogeneity
#' 
#' @author Sixiang Hu
#' @importFrom data.table setkey data.table
#' @importFrom plotly plot_ly add_histogram2dcontour add_trace %>%
#' @export resiPlot
#' @examples
#' set.seed(1L)
#' act <- rgamma(10000,1)
#' pred <- rgamma(10000,1) + sample(c(0.001,0),10000,replace=TRUE)
#' resiPlot(act,pred)

resiPlot <- function(act,pred,weight=NULL,exposure=NULL,bucket=20){
  rng <- range(rbind(act,pred),na.rm = TRUE,finite=TRUE)
  
  if (is.na(rng) || is.infinite(rng)) stop("Given data are all NAs or infinites.")
  
  by <- ( rng[2] -rng[1] ) / bucket
  
  pred_cuts <- cut(pred,breaks=seq(rng[1] + by/2,rng[2] - by/2,by=by),
              ordered_result = TRUE,include.lowest = TRUE)

  if (length(act) != length(weight) && length(weight) >0 ){
    weight <- NULL
    warning("Given weight has different length as actual value, will be ingored.")
  }
    
  if (is.null(weight)) weight <- rep(1,length(act))
  if (is.null(exposure)) exposure <- rep(1,length(act))

  temp <- data.table::data.table(act,pred,pred_cuts,weight,exposure)
  
  temp2 <- temp[,lapply(.SD,function(x,w,e)sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE),e=exposure,w=weight),
       by=pred_cuts,.SDcols=c("act","pred")][order(pred_cuts)]

  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="Actual", 
              linecolor = "#000000", gridcolor = "#E5E5E5")

  ax <- list(title="Predicted", showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5")
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  
  #aveplot
  rng <- range(c(temp2$act,temp2$pred), na.rm = TRUE, finite = TRUE)
  
  ave <- plotly::plot_ly(data=temp2) %>%
    plotly::add_trace(x=~pred,y=~act,name="Fitted", type = 'scatter',
                      marker=list(color="blue"),mode = 'markers') %>%
    plotly::add_trace(x=rng, y=rng, type = 'scatter',name="Reference",
                      line=list(color="red"),mode = 'lines')

  #residual
  res <- data.table::data.table(res=act - pred,pred=pred)

  resP <- plotly::plot_ly(data=res)%>%
    plotly::add_histogram2dcontour(x=~pred,y=~res)
  
  plotly::subplot(
    ave,
    resP,
    nrows = 2, heights = c(0.6, 0.4),
    shareX = TRUE, shareY = TRUE
  )
}

#global variable
globalVariables(c("x","e","w"))
