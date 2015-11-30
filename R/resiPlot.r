#' resiPlot
#'
#' @description This function assess the residual using given actual and predicted values.
#' @usage resiPlot(act,pred,bucket=20)
#' @param act numerical vector for actual observation.
#' @param pred numerical vector for model preidctions.  It must have the same length as act.
#' @param bucket Integer. It specifies the number of bucket of the AvsE plot.
#' @details 
#' Currently, the residual in this function is defined as: Residual = actual - predicted.  I don't use `resi` or `reisdual` function
#' from `stats` package because this function will be used for much wider model assess (e.g. `randomFoest`, `gbm`), but 2 functions
#' mentioned about can only applied to `glm` and `lm`.  May code in some other residual function.
#'
#' This function will give 3 plots:
#' Residual vs Prediction: This plot is used to assess the baise and heterogeneity
#' Residual histogram: Check the residual distribution.
#' AvsE plot: The aggregated (by provided number of bucket) average actual and predicted value, with a diagnal line for comparison.
#' 
#'  
#' @author Sixiang Hu
#' @importFrom ggplot2 aes ggplot
#' @importFrom grid grid.newpage viewport pushViewport
#' @export resiPlot
#' 

resiPlot <- function(act,pred,bucket=20){
  rng <- range(rbind(act,pred),na.rm = TRUE,finite=TRUE)
  
  if (is.na(rng) || is.infinite(rng)) stop("Given data is all NA's or infinite's.")
  
  by <- ( rng[2] -rng[1] ) / bucket
  
  label <- seq(rng[1] + by/2,rng[2] - by/2,by=by)
  
  cuts <- cut(pred,breaks=seq(rng[1],rng[2],by=by),
              ordered_result = TRUE,include.lowest = TRUE)
  
  df <- data.frame(act,cuts)
  df_act <- aggregate(df$act,by=list(df$cuts),FUN=mean,na.rm = TRUE)
  df_act <- cbind(df_act,label)
  
  AvsE <- ggplot2::ggplot(df_act) + 
    ggplot2::geom_abline(a=0,b=1,size=1,color="red")+
    ggplot2::geom_point(aes(x=label,y=x),color="light blue",size=8) +
    ggplot2::theme_bw()+
    ggplot2::xlab("Predicted")+
    ggplot2::ylab("Actual Observations")
  
  #residual
  res <- data.frame(res=act - pred,pred=pred)
  
  Hist <- ggplot2::ggplot(res)+
    ggplot2::geom_histogram(aes(x=res),binwidth=by,color="black",fill="yellow")+
    ggplot2::scale_x_continuous(breaks=seq(rng[1],rng[2],by=by))+
    ggplot2::theme_bw()+
    ggplot2::xlab("Residuals")+
    ggplot2::ylab("")    
  
  res_point <-ggplot2::ggplot(res)+
    ggplot2::geom_point(aes(x=pred,y=res),color="light blue",size=2)+
    ggplot2::theme_bw()+
    ggplot2::xlab("Predicted")+
    ggplot2::ylab("Residuals (Actual - Expected)")    
  
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 2)))
  print(res_point, vp = grid::viewport(layout.pos.row = 1:3, layout.pos.col = 1:2))
  print(Hist, vp = grid::viewport(layout.pos.row = 4, layout.pos.col = 1:1)) 
  print(AvsE, vp = grid::viewport(layout.pos.row = 4, layout.pos.col = 2:2))
}
