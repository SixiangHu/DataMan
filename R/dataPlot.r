#' dataPlot: visualise observation
#'
#' @description This function allows you to visualise features of a dataset 
#' by specifying dependent and response variable.
#' @usage dataPlot(x,y,by=NULL,weights=NULL, exposure = NULL,newGroupNum=10,xlim=NULL, 
#'     breaks = NULL, missing=TRUE, xname="x",yname="Response",byname="by",legendPos=NULL)
#' @param x a vector indicates the dependent variable
#' @param y a vector indicates the response variable
#' @param weights Optional. A numerical vector to specify the weights used for calculating weighted average of response.
#' normally this is the figures from over/down-samping.
#' @param exposure Optional. A numerical vector to specify the exposure used for calculating weighted average of response.
#' @param by Optinal.A numerical vector to specify the <by> variable
#' @param newGroupNum An integer specifies number of new bands 
#' when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @param xlim Optional. A vector provides the range of the variable e.g. xlim=c(0,100).
#' @param xname,yname,byname Optional. Characters of axis to be shown on plot.
#' @param breaks Optional. A vector to specify the breaks for the xvar.
#' @param missing logical. whether to show the `NA` as `Missing` in plot.  
#' If a `Missing` level is already existed, then `NA` will combined
#' @param legendPos Optional. 2 numeric element vector specify the location of legend.
#'  
#' @author Sixiang.Hu
#' 
#' @importFrom data.table as.data.table data.table setkey := uniqueN
#' @importFrom plotly plot_ly add_trace add_markers add_bars layout %>% add_lines
#' @importFrom checkmate testDate
#' @export
#' @examples
#' 
#' dataPlot(mtcars$gear,mtcars$mpg,mtcars$wt)

dataPlot <- function(x,y,by=NULL,weights=NULL, exposure = NULL,newGroupNum=10, xlim=NULL, 
                     breaks = NULL, missing=TRUE, xname="x",yname="Response",byname="by",
                     legendPos = NULL){
  # Error Trap
  if( is.null(x) ) stop("xvar provided is null.") 
  if( is.null(y) ) stop("yvar provided is null.")
  if (length(x) != length(y)) stop("x and y don't have the same length")
  if( !is.null(by) ) if (length(x) != length(by)) stop("x and by don't have the same length")

  # range index
  if ( is.null(xlim) ) {
    ind <- 1:length(x)
  }
  else if ( !is.null(xlim) && !(is.numeric(x) || testDate(x)) ) {
    ind <- 1:length(x)
    warning("xlim is provided on character variable. Ignored")
  }
  else if (length(xlim)>2 || length(xlim)<2) {
    ind <- 1:length(x)
    warning("xlim provided must has 2 elements.")
  }
  else ind <- which(x>=xlim[1] && x<=xlim[2])

  # weights
  if( !is.null(weights) ){
    if (length(x) != length(weights)) 
      stop("x and weights don't have the same length")
    else
      w <- weights
  }else w <- rep(1,length(x))
  
  # exposure
  if( !is.null(exposure) ){
    if (length(x) != length(exposure)) 
      stop("x and weights don't have the same length")
    else
      e <- exposure
  }else e <- rep(1,length(x))
  
  #New group for x if it has too many levels.
  if (is.numeric(x) && nlevels(as.factor(x)) > 100) {
    if(is.null(breaks)) breaks <- dmBreak(x, newGroupNum)
    x <- cut(x, breaks, include.lowest = TRUE, ordered_result = TRUE)
  }

  #New Group for byvar if it has too many levels.
  if(!is.null(by)){
    if ( is.integer(by)& uniqueN(by[ind])>20 ) {
      new_band <- dmBreak(by,newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE,ordered_result=TRUE)
    }
  }
  
  #Title for plot
  strTitle <- paste("Observation Analysis on: ",xname)

  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title=yname, 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  
  ay2 <- list(side = "right", showgrid=FALSE, title="Exposure(%)",
              linecolor = "#000000")
  
  if(is.character(x)) {
    ax <- list(title="", showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5",type = "category",
               categoryorder = "category ascending")
  }else{
    ax <- list(title="", showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5")
  }
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h",x=legendPos[1],y=legendPos[2])
  m <- list(l=-5,r=-5,b=-5,t=-5,pad=0)
    
  if (is.null(by)) {
    data.plot <- data.table(x=x[ind],y=y[ind],w=w[ind],e=e[ind])
    setkey(data.plot,x)
    
    if(missing){
      set(data.plot,i=which(is.na(data.plot[["x"]])),j="x",value="Missing")
    }
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=x,.SDcols=c("y","w","e")]
    data.agg  <- data.plot[,lapply(.SD,function(x,w,e) sum(y*w,na.rm=TRUE)/sum(e*w,na.rm=TRUE),e=e,w=w),by=x,.SDcols=c("y","w","e")]
    data.hist <- data.plot[,sum(e),by=x][,freq:=round(V1/sum(V1)*100,1)][order(x)]

    plot_ly(data=data.agg, x=~x, y=~y, name="Observed") %>%
      add_lines(line=list(color="#CC3399"),yaxis="y1") %>%
      add_markers(marker=list(color="#CC3399",symbol="square",size=10),showlegend=FALSE) %>%
      add_bars(x=~x,y=~freq,data=data.hist,showlegend=FALSE,
                        marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                        opacity=0.5,yaxis = "y2") %>%
      layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2 = c(ay2,list(range=c(0,min(max(data.hist$freq)*2.5,100)))), 
                     legend=l,margin = m)
  }
  else{
    strTitle  <- paste(strTitle," by ",byname,sep="")
    
    data.plot <- data.table(x=x[ind],y=y[ind],w=w[ind],e=e[ind],by=by[ind])
    setkey(data.plot,x,by)
    
    if(missing){
      set(data.plot,i=which(is.na(data.plot[["x"]])),j="x",value="Missing")
      set(data.plot,i=which(is.na(data.plot[["by"]])),j="by",value="Missing")
    }
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(x,by),.SDcols=c("y","w","e")]
    data.agg  <- data.plot[,lapply(.SD,function(x,w,e) sum(y*w,na.rm=TRUE)/sum(e*w,na.rm=TRUE),e=e,w=w),by=list(x,by),.SDcols=c("y","w","e")]
    data.hist <- data.plot[,sum(e),by=list(x,by)][,freq:=round(V1/sum(V1)*100,1)][order(by,x)]
    
    plot_ly(data=data.agg,x=~x,y=~y,color=~paste0("Observed: ",by)) %>%
        add_lines(yaxis="y1") %>%
        add_bars(x=~x, y=~freq, color=~paste0("Observed: ",by),colors='Set1', data=data.hist,
                          marker=list(line=list(color="#606060", width=1.5)),
                          showlegend=FALSE, opacity=0.5, yaxis="y2") %>%
        layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2 = c(ay2,list(range=c(0,min(max(data.hist$freq)*2.5,100)))), 
                       legend=l, barmode="stack", margin=m)
  }
}
