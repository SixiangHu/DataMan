#' dataPlot
#'
#' @description This function allows you to visualise features of a dataset 
#' by specifying dependent and response variable.
#' @usage dataPlot(data,xvar,yvar,byvar=NULL,weights=NULL,newGroupNum=10)
#' @param data a data frame.
#' @param xvar either an integer to specify the position of the dependent variable in the data frame, 
#' or a character string to indicate the dependent variable name.
#' @param yvar either an integer to specify the position of the response variable in the data frame, 
#' or a character string to indicate the response variable name.
#' @param weights Optional. One of: 
#' a numerical vector to specify the weights used for calculating weighted average of response,
#' a character string to specify the name of weight variable in the data frame,
#' an integer to specify the position of the weight variable in the data frame.
#' @param byvar Optinal. either an integer to specify the position of the <by> variable in the data frame, 
#' or a character string to indicate the <by> variable name.
#' @param newGroupNum An integer specifies number of new bands 
#' when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @details 
#' Before entering modelling stage, we may want to go through variable by variable in a data set to find the 
#' features for response variable. This function provides this functionality.
#' 
#' \code{\link{modelPlot}} function in this package provides similar function, but \code{\link{modelPlot}} can 
#' only visualise the feature in a specific model. 
#' What's more, \code{\link{modelPlot}} takes longer time to create plots.
#'  
#' @author Sixiang Hu
#' @importFrom data.table as.data.table data.table setkey :=
#' @importFrom plotly plot_ly add_trace layout
#' @export dataPlot
#' @examples
#' 
#' dataPlot(mtcars,"wt","mpg")
#' 
#' dataPlot(mtcars,"wt","mpg",byvar="vs")

dataPlot <- function(data,xvar,yvar,byvar=NULL,weights=NULL,
                     newGroupNum=10){
  
  # Error Trap
  if( .isDFnull(data) ) stop("data set provided is null.")
  if( is.null(xvar) ) stop("X variable provided is null.") 
  if( is.null(yvar) ) stop("Y variable provided is null.")
  
  # Find data column
  posi <- .VarPosition(data,xvar)
  x <- data[[posi$posi]]
  xname <- posi$name
  
  posi <- .VarPosition(data,yvar)
  y <- data[[posi$posi]]
  yname <- posi$name
  
  if( !is.null(byvar) ){
    posi <- .VarPosition(data,byvar)
    by <- data[[posi$posi]]
    byname <- posi$name
  }
  else by <- NULL
  
  # weights
  if( !is.null(weights) ){
    if (is.integer(weights) && length(weights)>1){
      if ( dim(data)[1] != length(weights) ) stop ("Length of weights is not the same as dimension of the data provided.")
      w <- weights
    }
    else{
      posi <- .VarPosition(data,weights)
      w <- data[[posi$posi]]
    }
  }
  else w <- rep(1,dim(data)[1])
  
  #New group for xvar if it has too many levels.
  if ( (is.numeric(x) || is.integer(x) ) && nlevels(as.factor(x))>=100 ) {
    new_band <- dmBreak(x,newGroupNum)
    x <- cut(x,new_band,include.lowest = TRUE,ordered_result=TRUE)
  }

  #New Group for byvar if it has too many levels.
  if(!is.null(by)){
    if ( (is.numeric(by) || is.integer(by)) && nlevels(as.factor(by))>20 ) {
      new_band <- dmBreak(by,newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE,ordered_result=TRUE)
    }
  }
  
  #Data for plot
  strTitle <- paste("Observation Analysis on: ",xname)
  
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title=yname, 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  
  ay2 <- list(side = "right", showgrid=FALSE, title="Weights",
              linecolor = "#000000")
  
  ax <- list(title=xname, showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5")
  
  l <- list(bordercolor = "#000000",borderwidth=1)

  if (is.null(by)) {
    data.plot <- data.table::data.table(x=x,y=y,w=w)
    data.table::setkey(data.plot,x)

    data.plot <- data.plot[,lapply(.SD,as.numeric),by=x,.SDcols=c("y","w")]
    data.agg <- data.plot[,lapply(.SD,weighted.mean,w=w),by=x,.SDcols=c("y","w")]
    data.hist <- data.plot[,sum(w),by=x][,freq:=V1/sum(V1)][order(x)]
    
    plotly::plot_ly(data=data.agg, x=x, y=y, line=list(color="#CC3399",shape="linear"),
                    marker=list(symbol="square",size=10), name="Observed",yaxis="y1") %>%
      plotly::add_trace(x=x,y=freq,data=data.hist,type="bar",showlegend=FALSE,
                        marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                        opacity=0.5,yaxis = "y2") %>%
      plotly::layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2=ay2, legend=l)
  }
  else{
    strTitle <- paste(strTitle," by ",byname,sep="")
    
    data.plot <- data.table::data.table(x=x,y=y,w=w,by=by)
    data.table::setkey(data.plot,x,by)

    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(x,by),.SDcols=c("y","w")]
    data.agg <- data.plot[,lapply(.SD,weighted.mean,w=w),by=list(x,by),.SDcols=c("y","w")]
    data.hist <- data.plot[,sum(w),by=list(x,by)][,freq:=V1/sum(V1)][order(by,x)]

    suppressWarnings(
      plotly::plot_ly(data=data.agg,x=x,y=y,color=paste("Observed",by,sep="-"),yaxis = "y1")%>%
        plotly::add_trace(x=x, y=freq, color=by, data=data.hist, type="bar",
                          marker=list(line=list(color="#606060", width=1.5)),
                          showlegend=FALSE, opacity=0.5, yaxis="y2") %>%
        plotly::layout(title=strTitle, legend=l, barmode="stack",
                       xaxis=ax, yaxis=ay1, yaxis2=ay2)
    )
  }
}