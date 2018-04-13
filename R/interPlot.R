#' interPlot
#'
#' @description Interactive 3D plot by calling `plotly` package functions.
#' @usage interPlot(data,xvar,yvar,zvar,wvar=NULL,numGrpx=10,numGrpy=10)
#' @param data a data frame.
#' @param xvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @param yvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @param zvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @param wvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @param numGrpx,numGrpy integer. Give the number of buckets for either x or y factors.
#' @details This functions gives a 3D interactive view of between factors.
#' This is really useful when modeller wants to assess the interaction terms.
#' Hence the "zvar" can be actual response data or model predictions.
#' @author Sixiang Hu
#' @importFrom data.table data.table setkey key := dcast
#' @importFrom plotly plot_ly layout %>%
#' @export interPlot
#' @examples
#' 
#' interPlot(mtcars,"wt","mpg","vs")

interPlot <- function(data,xvar,yvar,zvar,wvar=NULL,numGrpx=10,numGrpy=10){
  # Error Trap
  if( .isDFnull(data) ) stop("data set provided is null.")
  if( is.null(xvar) ) stop("X variable provided is null.") 
  if( is.null(yvar) ) stop("Y variable provided is null.")
  if( is.null(zvar) ) stop("Z variable provided is null.")
  
  # Find data column
  posi <- .VarPosition(data,xvar)
  x <- data[[posi$posi]]
  if ( (is.numeric(x) || is.integer(x) ) && nlevels(as.factor(x))>=100 ) {
    new_band <- dmBreak(x,numGrpx)
    x <- cut(x,new_band,include.lowest = TRUE,ordered_result = TRUE)
  }

  posi <- .VarPosition(data,yvar)
  y <- data[[posi$posi]]
  if ( (is.numeric(y) || is.integer(y) ) && nlevels(as.factor(y))>=100 ) {
    new_band <- dmBreak(y,numGrpy)
    y <- cut(y,new_band,include.lowest = TRUE,ordered_result = TRUE)
  }

  posi <- .VarPosition(data,zvar)
  z <- data[[posi$posi]]
  
  if( is.null(wvar)) {
    w <- rep(1,dim(data)[1])
  }
  else {
    posi <- .VarPosition(data,wvar)
    w <- data[[posi$posi]]
  }
  
  dt <- data.table::data.table(x = x,y = y,z = z,w = w)
  data.table::setkey(dt,x,y)
  
  dt2  <- dt[,lapply(.SD,weighted.mean,w=w),by=list(x,y),.SDcols=c("z","w")]
  dt2 <- data.table::dcast(dt2,x~y,value.var="z",drop=FALSE)
  
  zvalue <- suppressWarnings(DataMan::PopMiss(as.matrix(dt2[,-1,with=FALSE]),
                                     na.treatment = "replace",0))

  plotly::plot_ly(x=~x,y=~y,z=~zvalue,type = "surface") %>%
    plotly::layout(scene=list(xaxis=list(title=paste0(xvar," (x)")),
                           yaxis=list(title=paste0(yvar," (y)")),
                           zaxis=list(title=paste0(zvar," (z)")))
                   )
}