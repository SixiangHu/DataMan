#' dataPlot
#'
#' @description This function allows you to visualise features of a dataset by specifying dependent and response variable.
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
#' @param newGroupNum An integer specifies number of new bands when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @details 
#' Before entering modelling stage, we may want to go through variable by variable in a data set to find the 
#' features for response variable. This function provides this functionality.
#' 
#' \code{\link{modelPlot}} function in this package provides similar function, but \code{\link{modelPlot}} can 
#' only visualise the feature in a specific model. What's more, \code{\link{modelPlot}} takes longer time to create plots.
#'  
#' @author Sixiang Hu
#' @importFrom data.table as.data.table setkey :=
#' @importFrom rbokeh figure ly_lines ly_points ly_hist grid_plot
#' @export dataPlot
#' @examples
#' 
#' dataPlot(mtcars,"wt","mpg")
#' 
#' dataPlot(mtcars,"wt","mpg",byvar="vs")

dataPlot <- function(data,xvar,yvar,byvar=NULL,weights=NULL,
                     newGroupNum=10){
  
  # Error Trapping
  if( is.null(data) ) stop("data set provided is null.")
  if( is.null(xvar) ) stop("X variable provided is null.") 
  if( is.null(yvar) ) stop("Responce variable provided is null.")
  
  # Find data column
  if (is.character(xvar)) {
    if(!xvar %in% colnames(data)) stop(paste("xvar variable (",xvar,") cannot be found.",""))
    x <- data[,which(names(data)==xvar)]
    xname <- xvar
  }
  else if (is.integer(xvar)) { 
    x <- data[,xvar]
    xname <- names(data)[xvar]
  }
  else stop ("xvar provided is either a character (variable name) or integer (position of the variable).")
  
  if (is.character(yvar)) {
    if(!yvar %in% colnames(data)) stop(paste("yvar variable (",yvar,") cannot be found.",""))
    y <- data[,which(names(data)==yvar)]
    yname <- yvar
  }
  else if (is.integer(yvar)) {
    y <- data[,yvar]
    yname <- names(data)[yvar]
  }
  else stop ("yvar provided is either a character (variable name) or integer (position of the variable).")
  
  if( !is.null(byvar) ){
    if (is.character(byvar)){
      if(!byvar %in% colnames(data) ) stop(paste("xvar variable (",byvar,") cannot be found.",""))
      by <- data[,which(names(data)==byvar)]
      byname <- byvar
    }
    else if (is.integer(byvar)) {
      xname <- names(data)[byvar]
      by <- as.character(data[,byvar])
    }
    else by <- NULL
  }
  else by <- NULL
  
  if( !is.null(weights) ){
    if (is.character(weights)){
      if(!weights %in% colnames(data) ) stop(paste("xvar variable (",weights,") cannot be found.",""))
      w <- data[,which(names(data)==weights)]
    }
    else if (is.integer(weights) && length(weights)==1) {
      w <- data[,weights]
    }
    else if (is.integer(weights) && length(weights)>1){
      if ( dim(data)[1] != length(weights) ) stop ("Length of weights is not the same as dimension of the data provided.")
      w <- weights
    }
  }
  else w <- rep(1,dim(data)[1])
  
  #New Group for xvar if it has too many levels.
  if ( (is.numeric(x) || is.integer(x) ) && nlevels(as.factor(x))>100 ) {
    new_band <- seq(min(x, na.rm = TRUE),max(x, na.rm = TRUE),length.out=newGroupNum)
    x <- cut(x,new_band,include.lowest = TRUE)
  }

  #New Group for byvar if it has too many levels.
  if(!is.null(by)){
    if ( (is.numeric(by) || is.integer(by)) && nlevels(as.factor(by))>20 ) {
      new_band <- seq(min(by, na.rm = TRUE),max(by, na.rm = TRUE),length.out=newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE)
    }
  }
  
  #Data for plot
  strTitle <- paste("Observation Analysis on: ",xname)

  if (is.null(by)) {
    data.plot <- data.table::data.table(x=x,y=y,w=w)
    data.table::setkey(data.plot,x)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=x,.SDcols=c("y","w")]
    data.agg <- data.plot[,lapply(.SD,weighted.mean,w=w),by=x,.SDcols=c("y","w")]
    data.agg <- data.agg[order(as.factor(x)),]

    rbokeh::figure(title = strTitle,ylab=yname,height = 500, width = 900) %>%
      rbokeh::ly_lines(as.factor(x),y,data=data.agg) %>%
      rbokeh::ly_points(as.factor(x),y,data=data.agg,glyph=0,
                        hover="<strong>x value:</strong> @x<br><strong>y value:</strong> @y")
    
    if (class(x) %in% c("integer","numeric","Date")) { 
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height = 250, width = 900) %>%
        rbokeh::ly_hist(x,breaks=nlevels(as.factor(x)))
    }
    else {
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height = 250, width = 900) %>%
        rbokeh::ly_bar(x[order(x)])
    }
    
    #rbokeh::grid_plot(list(list(p1),list(p2)),nrow=2,ncol=1,same_axes = c(TRUE, FALSE))
  }
  else{
    data.plot <- data.table::as.data.table(as.data.frame(cbind(x=x,y=y,w=w,by=by),stringsAsFactors=FALSE))
    data.table::setkey(data.plot,x,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(x,by),.SDcols=c("y","w")]
    data.agg <- data.plot[,lapply(.SD,weighted.mean,w=w),by=list(x,by),.SDcols=c("y","w")]
 
    #line graph
    p1 <- rbokeh::figure(title = strTitle,xlab="",ylab=yname,height = 500, width = 900) %>%
      rbokeh::ly_lines(x,y,data=data.agg,group=by,color=by) %>%
      rbokeh::ly_points(x,y,data=data.agg,group=by,color=by,
                        hover="<strong>x value:</strong> @x<br><strong>y value:</strong> @y<br><strong>by value:</strong> @by")
    
    if (class(x) %in% c("integer","numeric","Date")) { 
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height = 250, width = 900) %>%
        rbokeh::ly_hist(x,breaks=nlevels(as.factor(x)))
    }
    else {
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height = 250, width = 900) %>%
        rbokeh::ly_bar(x,color=by,data=data.plot)
    }
    
    grid_plot(list(p1,p2),nrow=2,ncol=1,byrow=TRUE,same_axes = c(TRUE, FALSE))
  }
}