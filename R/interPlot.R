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
#' @import data.table
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
  
  dt <- data.table(x = x,y = y,z = z,w = weight,e=exposure)
  setkey(dt,x,y)

  dt <- dt[,.(z = sum(z*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE)), by=list(x,y)]
  dt <- dcast(dt,y~paste0(x),value.var="z",drop=FALSE,fill=0)

  Response <- as.matrix(dt[,-"y",with=FALSE])
  rownames(Response) <- dt[["y"]]
  colnames(Response) <- colnames(dt)[-1]

  custom_hover <- array(dim=dim(Response))
  for (x in seq_len(ncol(Response))) {
    for (y in seq_len(nrow(Response))) {
      custom_hover[(x-1)*nrow(Response) + y] = paste(yname,': ', rownames(Response)[y], '<br />',
                                                     xname,': ', colnames(Response)[x], '<br />',
                                                     zname,': ', Response[(x-1)*nrow(Response) + y])
    }
  }

  plot_ly(z=~Response,hoverinfo="text",text=custom_hover) %>%
    add_surface() %>%
    layout(scene=list(xaxis=list(title=paste0(xname," (x)"),ticketmode = 'array',ticktext = colnames(Response),tickvals=0:(ncol(Response)-1)),
                      yaxis=list(title=paste0(yname," (y)"),ticketmode = 'array',ticktext = rownames(Response),tickvals=0:(nrow(Response)-1)),
                      zaxis=list(title=paste0(zname," (z)"))),
           legend = list(legendgroup="")
           )
}

#global variable
globalVariables(c("e","w","."))