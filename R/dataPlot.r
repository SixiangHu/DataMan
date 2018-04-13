#' dataPlot
#'
#' @description This function allows you to visualise features of a dataset 
#' by specifying dependent and response variable.
#' @usage dataPlot(x,y,by=NULL,weights=NULL,newGroupNum=10,xlim=NULL,xname="x",yname="y",byname="by")
#' 
#' @param x a vector indicates the dependent variable
#' @param y a vector indicates the response variable
#' @param weights Optional. A numerical vector to specify the weights used for calculating weighted average of response,
#' @param by Optinal.A numerical vector to specify the <by> variable
#' @param newGroupNum An integer specifies number of new bands 
#' when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @param xlim Optional. A vector provides the range of the variable e.g. xlim=c(0,100).
#' @param xname,yname,byname Optional. Characters of axis to be shown on plot.
#' @details 
#' Before entering modelling stage, we may want to go through variable by variable in a data set to find the 
#' features for response variable. This function provides this functionality.
#' 
#' \code{\link{modelPlot}} function in this package provides similar function, but \code{\link{modelPlot}} can 
#' only visualise the feature in a specific model. 
#' What's more, \code{\link{modelPlot}} takes longer time to create plots.
#'  
#' @author Sixiang Hu
#' @importFrom data.table as.data.table data.table setkey := uniqueN
#' @importFrom plotly plot_ly add_lines add_markers add_bars layout %>%
#' @export dataPlot
#' @examples
#' 
#' dataPlot(mtcars$gear,mtcars$mpg)
#' 
#' dataPlot(mtcars$gear,mtcars$mpg,mtcars$wt)

dataPlot <- function(x,y,by=NULL,weights=NULL,newGroupNum=10, xlim=NULL,
                      xname="x",yname="y",byname="by"){
  
  # Error Trap
  if( is.null(x)  ) stop("xvar provided is null.") 
  if( is.null(y)  ) stop("yvar provided is null.")
  
  # Find data column
  if (length(x) != length(y)) stop("x and y don't have the same length")

  if( !is.null(by) ){
    if (length(x) != length(by)) stop("x and by don't have the same length")
  }

  # range index
  if ( is.null(xlim) ) {
    ind <- 1:length(x)
  }
  else if ( !(is.numeric(x) || is.integer(x) || inherits(x, 'Date')) ) {
    ind <- 1:length(x)
    warning("xlim is provided on character variable. Ignored")
  }
  else if (length(xlim)!=2 ) {
    ind <- 1:length(x)
    warning("xlim provided must has 2 elements.")
  }
  else ind <- which(x>=xlim[1] & x<=xlim[2])
  
  # weights
  if( !is.null(weights) ){
    if (length(x) != length(weights)) {
      stop("x and weights don't have the same length")
    }
    else w <- weights[ind]
  }
  else w <- rep(1,length(ind))
  
  #New group for x if it has too many levels.
  if ( (is.numeric(x) || is.integer(x) ) && data.table::uniqueN(x[ind])>=100 ) {
    new_band <- dmBreak(x[ind],newGroupNum)
    x <- cut(x[ind],new_band,include.lowest = TRUE,ordered_result=TRUE)
  }

  #New Group for byvar if it has too many levels.
  if(!is.null(by)){
    if ( (is.numeric(by) || is.integer(by)) && data.table::uniqueN(by[ind])>20 ) {
      new_band <- dmBreak(by[ind],newGroupNum)
      by <- cut(by[ind],new_band,include.lowest = TRUE,ordered_result=TRUE)
    }
    if (is.numeric(by)) by <- by[ind]
  }
  
  #Title for plot
  strTitle <- paste("Observation Analysis on: ",xname)

  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title=yname, 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  
  ay2 <- list(side = "right", showgrid=FALSE, title="Weights(%)",
              linecolor = "#000000")
  
  if(is.character(x)) {
    ax <- list(title=xname, showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5",type = "category",
               categoryorder = "category ascending")
  }
  else{
    ax <- list(title=xname, showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5")
  }
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  m <- list(l=-5,r=-5,b=-5,t=-5,pad=0)
    
  if (is.null(by)) {
    data.plot <- data.table::data.table(x=x[ind],y=y[ind],w=w[ind])
    data.table::setkey(data.plot,x)

    data.plot <- data.plot[,lapply(.SD,as.numeric),by=x,.SDcols=c("y","w")]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=w),by=x,.SDcols=c("y","w")]
    data.hist <- data.plot[,sum(w),by=x][,freq:=round(V1/sum(V1)*100)][order(x)]

    plotly::plot_ly(data=data.agg, x=~x, y=~y, name="Observed") %>%
      plotly::add_lines(line=list(color="#CC3399"),yaxis="y1") %>%
      plotly::add_markers(marker=list(color="#CC3399",symbol="square",size=10),showlegend=FALSE) %>%
      plotly::add_bars(x=~x,y=~freq,data=data.hist,showlegend=FALSE,
                        marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                        opacity=0.5,yaxis = "y2") %>%
      plotly::layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2=ay2, 
                     legend=l,margin = m)
  }
  else{
    strTitle  <- paste(strTitle," by ",byname,sep="")
    
    data.plot <- data.table::data.table(x=x[ind],y=y[ind],w=w[ind],by=by[ind])
    data.table::setkey(data.plot,x,by)

    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(x,by),.SDcols=c("y","w")]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=w),by=list(x,by),.SDcols=c("y","w")]
    data.hist <- data.plot[,sum(w),by=list(x,by)][,freq:=round(V1/sum(V1)*100)][order(by,x)]
    
    by_len <- length(unique(by[ind]))
    if(by_len<3){
      color_pal <- c("seagreen4","dodgerblue2")
    }else{
      color_pal <-"Set1"
    }

    plotly::plot_ly(data=data.agg,x=~x,y=~y,color=~paste0("Observed: ",by),colors=color_pal) %>%
      plotly::add_lines(yaxis="y1") %>%
      plotly::add_bars(x=~x, y=~freq, color=~paste0("Observed: ",by),colors=color_pal, data=data.hist,
                        marker=list(line=list(color="#606060", width=1.5)),
                        showlegend=FALSE, opacity=0.5, yaxis="y2") %>%
      plotly::layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2=ay2, 
                     legend=l, barmode="stack",margin = m)

  }
}