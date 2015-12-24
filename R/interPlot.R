#' interPlot
#'
#' @description Interactive 3D plot by calling `plotly` package functions.
#' @usage interPlot(data,xvar,yvar,zvar)
#' @param data a data frame.
#' @param xvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @param yvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @param zvar either an integer to specify the position of the variable in the data frame, or the name of the variable.
#' @details This functions gives a 3D interactive view of between factors.
#' This is really useful when modeller wants to assess the interaction terms.
#' Hence the "zvar" can be actual response data or model predictions.
#' @author Sixiang Hu
#' @importFrom data.table data.table setkey key := dcast
#' @importFrom plotly plot_ly layout
#' @export interPlot
#' @examples
#' 
#' interPlot(mtcars,"wt","mpg","vs")

interPlot <- function(data,xvar,yvar,zvar){
  # Error Trap
  if( .isDFnull(data) ) stop("data set provided is null.")
  if( is.null(xvar) ) stop("X variable provided is null.") 
  if( is.null(yvar) ) stop("Y variable provided is null.")
  if( is.null(zvar) ) stop("Z variable provided is null.")
  
  # Find data column
  posi <- .VarPosition(data,xvar)
  x <- data[[posi$posi]]

  posi <- .VarPosition(data,yvar)
  y <- data[[posi$posi]]

  posi <- .VarPosition(data,zvar)
  z <- data[[posi$posi]]
  
  dt <- data.table::data.table(x = x,
                   y = y,
                   z = z)
  data.table::setkey(dt,x,y)
  
  dt2 <- data.table::dcast(dt,x~y,fun.aggregate=mean,
                           na.rm=TRUE,
                           value.var="z",
                           drop=FALSE)
  
  zvalue <- suppressWarnings(DataMan::PopMiss(as.matrix(dt2[,-1,with=FALSE]),
                                     na.treatment = "replace",
                                     0))

  plotly::plot_ly(
          x=dt2[[1]],
          y=colnames(dt2),
          z=zvalue, 
          type = "surface",
          showlegend=FALSE) %>%
    plotly::layout(xaxis=list(title=xvar),
           yaxis=list(title=yvar),
           zaxis=list(title=zvar))
}