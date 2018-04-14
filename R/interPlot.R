#' interPlot
#'
#' @description Interactive 3D plot by calling `plotly` package functions.
#' @usage interPlot(x,y,z,weight=NULL,exposure=NULL,numGrpx=10,numGrpy=10,
#' xname="x",yname="y",zname="z")
#' @param x,y a vector indicates the dependent variable that you want to visulise on (i.e. Age)
#' @param z a vector indicates the response variable that you want to visulise on (i.e. Age)
#' @param weight Optional. A numerical vector to specify the weights used for calculating weighted average of response.
#' normally this is the figures from over/down-samping.
#' @param exposure Optional. A numerical vector to specify the exposure used for calculating weighted average of response.
#' @param numGrpx,numGrpy integer. Give the number of buckets for either x or y factors.
#' @param xname,yname,zname Optional. Characters to be shown on plot.
#' @details This functions gives a 3D interactive view of between factors.
#' This is really useful when modeller wants to assess the interaction terms.
#' Hence the `z` can be actual response data or model predictions.
#' @author Sixiang Hu
#' @importFrom data.table data.table setkey key := dcast
#' @importFrom plotly plot_ly layout add_surface %>%
#' @export interPlot
#' @examples
#' 
#' interPlot(mtcars$disp,mtcars$mpg,mtcars$qsec)

interPlot <- function(x,y,z,weight=NULL,exposure=NULL,numGrpx=10,numGrpy=10,
                      xname="x",yname="y",zname="z"){
  # Error Trap
  if (is.null(x)) stop("x provided is blank.")
  if (is.null(y)) stop("y provided is blank.")
  if (is.null(z)) stop("z provided is blank.")
  
  nr <- length(x)
  
  if ( is.numeric(x) && nlevels(as.factor(x))>=100 ) {
    new_band <- dmBreak(x,numGrpx)
    x <- cut(x,new_band,include.lowest = TRUE,ordered_result = TRUE)
  }

  if ( is.numeric(y) && nlevels(as.factor(y))>=100 ) {
    new_band <- dmBreak(y,numGrpy)
    y <- cut(y,new_band,include.lowest = TRUE,ordered_result = TRUE)
  }

  if(is.null(weight)) weight <- rep(1,nr)
  if(is.null(exposure)) exposure <- rep(1,nr)
  
  dt <- data.table::data.table(x = x,y = y,z = z,w = weight,e=exposure)
  data.table::setkey(dt,x,y)

  dt2 <- dt[,lapply(.SD,function(x,w,e) sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE),e=e,w=w),by=list(x,y),.SDcols="z"]
  dt2 <- as.matrix(data.table::dcast(dt2,x~y,value.var="z",drop=FALSE,fill=0)[,-1])

  plotly::plot_ly(z=~dt2) %>%
    plotly::add_surface() %>%
    plotly::layout(scene=list(xaxis=list(title=paste0(xname," (x)")),
                           yaxis=list(title=paste0(yname," (y)")),
                           zaxis=list(title=paste0(zname," (z)")))
                   )
}

#global variable
globalVariables(c("e","w"))