#' resiPlot
#' 
#' @description This function assess the residual using given actual and
#'   predicted values.
#' @usage resiPlot(act,pred,weight=NULL,exposure=NULL,bucket=20,residualFun = "-",...)
#' 
#' @param act numerical vector for actual observation.
#' @param pred numerical vector for model preidctions.  It must have the same
#'   length as act.
#' @param weight numerical vector to give weight to observations.
#' @param exposure numerical vector to give exposure to observations.
#' @param bucket Integer. It specifies the number of bucket of the AvsE plot.
#' @param residualFun Character. Name of residual function. The first parameter 
#' must be actual, and second parameter must be predictions. By default, residual 
#' function is difference between `act` and `pred`.
#' @param ... other parameters for `residualFun` specified.
#' 
#' @details In previous version of this function, the residual in this function is defined as: Residual =
#' actual - predicted.  `resi` or `reisdual` function from `stats`
#' package are not used because this function will be used for much wider model assess (e.g.
#' `randomFoest`, `gbm`), and 2 functions mentioned above can only applied to
#' `glm` and `lm`.
#' 
#' This function will give 2 plots: 
#' AvsE plot: The average actual and predicted value for each bucket, with a diagnal line for comparison. 
#' Residual vs Prediction: This plot is used to assess the baise and heterogeneity
#' 
#' @author Sixiang Hu
#' @import data.table
#' @importFrom plotly plot_ly add_histogram2dcontour add_trace %>% subplot
#' @importFrom stats var
#' @export resiPlot
#' @examples
#' set.seed(1L)
#' act <- rgamma(10000,1)
#' pred <- act + rnorm(10000)
#' signDeviance <- function(y,mu){
#'   sign(y-mu)*sqrt(residualDeviance(y,mu,family = "gamma"))
#' }
#' resiPlot(act,pred,residualFun = "signDeviance")

resiPlot <- function(act,pred,weight=NULL,exposure=NULL,bucket=20,residualFun = "-",...){
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

  temp <- data.table(act,pred,pred_cuts,weight,exposure)
  temp2 <- temp[,.(act=sum(act*weight,na.rm = TRUE)/sum(exposure*weight,na.rm = TRUE),
                   pred=sum(pred*weight,na.rm = TRUE)/sum(exposure*weight,na.rm = TRUE),
                   stderr = sqrt(var(pred,na.rm=TRUE)/length(na.omit(pred)))),
                by=pred_cuts][order(pred_cuts)]
  #set axis
  ay1 <- list(overlaying = "y1", side = "left", title="Actual", 
              linecolor = "#000000", gridcolor = "#E5E5E5")

  ax <- list(title="Predicted", showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5",rangemode = "tozero")
  
  ay2 <- list(overlaying = "y2", side = "left", title="Residual", 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h",x=0.2,y=0.43)
  
  #aveplot
  rng <- range(c(temp2$act,temp2$pred), na.rm = TRUE, finite = TRUE)
  
  ave <- plot_ly(data=temp2) %>%
    add_trace(x=~pred,y=~act,name="Fitted", type = 'scatter',
                      marker=list(color="blue"),mode = 'markers',error_x = ~list(value = stderr)) %>%
    add_trace(x=rng, y=rng, type = 'scatter',name="Reference",
                      line=list(color="red"),mode = 'lines') %>%
    layout(title="Residual Analysis",xaxis=ax,yaxis = ay1, legend=l)

  #residual
  res <- data.table(res=do.call(residualFun,list(act,pred,...)),pred=pred)

  resP <- plot_ly(data=res)%>%
    add_histogram2dcontour(x=~pred,y=~res,colorscale="solar",name = "Residual")%>%
    add_trace(x=rng, y=0, type = 'scatter',name="Reference",
              line=list(color="black"),mode = 'lines') %>%
    layout(xaxis=ax, yaxis = ay2, legend=l)
  
  subplot(ave,resP,nrows = 2, heights = c(0.6, 0.4),shareX = TRUE, shareY = TRUE)
}

#global variable
globalVariables(c("x","e","w","."))
