#' resiPlot
#' 
#' @description This function assess the residual using given actual and
#'   predicted values.
#' @usage resiPlot(act,pred,weight=NULL,bucket=20)
#' @param act numerical vector for actual observation.
#' @param pred numerical vector for model preidctions.  It must have the same
#'   length as act.
#' @param weight numerical vector to give weight to observations.
#' @param bucket Integer. It specifies the number of bucket of the AvsE plot.
#' @details Currently, the residual in this function is defined as: Residual =
#' actual - predicted.  I don't use `resi` or `reisdual` function from `stats`
#' package because this function will be used for much wider model assess (e.g.
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
#' @importFrom plotly plot_ly add_lines add_histogram2dcontour add_markers %>%
#' @importFrom stats weighted.mean
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
  
  cuts <- cut(label,breaks=seq(rng[1],rng[2],by=by),
                            ordered_result = TRUE,include.lowest = TRUE)
  
  if (length(act) != length(weight) && length(weight) >0 ){
    weight <- NULL
    warning("Given weight has different length as actual value, will be ingored.")
  }
    
  if (is.null(weight)) weight <- rep(1,length(act))

  temp <- data.table::data.table(act,act_cuts,weight)
  data.table::setkey(temp,act_cuts)
  temp[,x:=weighted.mean(act,weight),by=act_cuts]
  temp_act <- unique(temp)

  temp_lab <- data.table::data.table(label,cuts)
  data.table::setkey(temp_lab,cuts)
  
  df_act <- temp_act[temp_lab]
   
  #AvsE plot
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="Actual", 
              linecolor = "#000000", gridcolor = "#E5E5E5")

  ax <- list(title="Predicted", showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5")
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  
  ave <- plotly::plot_ly(data=df_act) %>%
    plotly::add_markers(x=~label,y=~x,color="blue",name="Fitted") %>%
    plotly::add_lines(x=c(0,1),y=c(0,1),color="red",name="Observed") 
  #%>% plotly::layout(title = "Residual Plot", xaxis=ax,yaxis = ay1,legend=l)

  #residual
  res <- data.frame(res=act - pred,pred=pred)
  
  resP <- plotly::plot_ly(data=res)%>%
    plotly::add_histogram2dcontour(x=~pred,y=~res)
  
  plotly::subplot(
    ave,
    resP,
    nrows = 2, heights = c(0.6, 0.4),
    shareX = FALSE, shareY = FALSE
  )
}

#global variable
globalVariables(c("x"))
