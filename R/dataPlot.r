#' dataPlot
#'
#' @description This function allows you to visualise features of a dataset by specifying dependent and response variable.
#' @usage dataPlot(data,xvar,yvar,byvar=NULL,weights=NULL,interactive=FALSE,newGroupNum=10,...)
#' @param data a data frame.
#' @param xvar either an integer to specify the position of the dependent variable in the data frame, 
#' or a character string to indicate the dependent variable name.
#' @param yvar either an integer to specify the position of the response variable in the data frame, 
#' or a character string to indicate the response variable name.
#' @param weights Optional. One of: 
#' a numerical vector to specify the weights used for calculating weighted average of response,
#' a character string to specify the name of weight variable in the data frame,
#' an integer to specify the position of the weight variable in the data frame.
#' @param interactive logical. Set true to use googleVis package plotting interactively, which can be used in shiny apps. Currently it doesn't work when using "by" method.
#' @param byvar Optinal. either an integer to specify the position of the <by> variable in the data frame, 
#' or a character string to indicate the <by> variable name.
#' @param newGroupNum Optional. An integer specifies number of new bands when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @param ... xlim and ylim can be used to set the range of the ggplot2 plot. For example, xlim=c(0,1) means restrict the xaxis within (0,1).  This does not work for goolgeVis interactive plot because, because, because it is interactive, which you can zoom in and out with your mouse. :)
#' @details 
#' Before entering modelling stage, we may want to go through variable by variable in a data set to find the 
#' features for response variable. This function provides this functionality.
#' 
#' \code{\link{modelPlot}} function in this package provides similar function, but \code{\link{modelPlot}} can 
#' only visualise the feature in a specific model. What's more, \code{\link{modelPlot}} takes longer time to create plots.
#'  
#' @author Sixiang Hu
#' @importFrom data.table as.data.table setkey :=
#' @importFrom ggplot2 aes element_text ggplot
#' @importFrom gridExtra grid grid.arrange
#' @importFrom reshape2 melt
#' @importFrom googleVis gvisComboChart
#' @importFrom scales percent date_format
#' @export dataPlot
#' @examples
#' 
#' dataPlot(mtcars,"wt","mpg")
#' 
#' dataPlot(mtcars,"wt","mpg",byvar="vs")

dataPlot <- function(data,xvar,yvar,byvar=NULL,weights=NULL,interactive=FALSE,newGroupNum=10,...){
  
  opts.list<-list(...)
  opts <- names(list(...))
  if("xlim" %in% opts) xlim<-opts.list$xlim
  if("ylim" %in% opts) ylim<-opts.list$ylim
  if("binwidth" %in% opts) binwidth<-opts.list$binwidth else binwidth <- 1
  
  # Error Trapping
  if( is.null(data) ) stop("data set provided is null.")
  if( is.null(xvar) ) stop("X variable provided is null.") 
  if( is.null(yvar) ) stop("Responce variable provided is null.")
  
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
    yname <- yvar
    y <- data[,which(names(data)==yvar)]
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
    wname = "w"
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
  
  #New Group for xvar which has too much levels.
  if ( (is.numeric(x) || is.integer(x) ) && nlevels(as.factor(x))>100 ) {
    if ( is.null(newGroupNum) ) newGroupNum <- 10
    
    new_band <- seq(min(x, na.rm = TRUE),max(x, na.rm = TRUE),length.out=newGroupNum)
    x <- cut(x,new_band,include.lowest = TRUE)
  }
  
  #New Group for byvar which has too much levels.
  if(!is.null(by)){
    if ( (is.numeric(by) || is.integer(by)) && nlevels(as.factor(by))>100 ) {
      if ( is.null(newGroupNum) ) newGroupNum <- 10
      
      new_band <- seq(min(by, na.rm = TRUE),max(by, na.rm = TRUE),length.out=newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE)
    }
  }
  
  #Data for plot
  if (is.null(by)) {
    data.plot <- data.table::as.data.table(as.data.frame(cbind(x=x,y=y,w=w),stringsAsFactors=FALSE))
    data.table::setkey(data.plot,x)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=x,.SDcols=c("y","w")]
    data.agg <- as.data.frame(data.plot[,lapply(.SD,weighted.mean,w=w),by=x,.SDcols=c("y","w")],row.names=c("xvar","weights","observed"))
    data.freq <- as.data.frame(data.plot[,sum(w),by=x][,freq:=V1/sum(V1)])
    
    data.melt <- reshape2::melt(data.agg[,-3],id=c("x"))
    
    #line graph
    
    strV1 <- paste("Observation Analysis on: ",xname)
    
    gLine <- ggplot2::ggplot(data=data.melt,aes(x=x,y=value)) + 
      ggplot2::geom_line(size=1,colour= "magenta3") + ggplot2::geom_point(size=4,fill="white",shape=22)
    if(("xlim" %in% opts) && is.numeric(data.melt$x)) gLine <-gLine + ggplot2::scale_x_continuous(limits=xlim)
    else if(("xlim" %in% opts) && !is.numeric(data.melt$x)) gLine <-gLine + ggplot2::scale_x_discrete(limits=xlim)
    else if(("xlim" %in% opts) && is(data.melt[,"x"],"Date")) gLine <-gLine + ggplot2::scale_x_date(label=date_format("%y%m"),limits=xlim)
    if("ylim" %in% opts) gLine <-gLine + ggplot2::ylim(ylim)
    gLine <- gLine + ggplot2::xlab("") + ggplot2::ylab(yname)+ ggplot2::ggtitle(strV1)+ theme_mp_line
    if(nlevels(as.factor(data.melt$x))>25) gLine <- gLine + ggplot2::theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))
    
    #histogram graph
    ghist <- ggplot2::ggplot(data=data.freq,aes(x=x,y=freq))+
      ggplot2::geom_histogram(stat="identity",colour="black",fill="yellow")
    if(("xlim" %in% opts) && is.numeric(data.melt$x)) ghist <-ghist + ggplot2::scale_x_continuous(limits=xlim)
    else if(("xlim" %in% opts) && !is.numeric(data.melt$x)) ghist <-ghist + ggplot2::scale_x_discrete(limits=xlim)
    ghist <- ghist + ggplot2::ylab("percent (%)")+
      ggplot2::scale_y_continuous(labels = percent)+
      ggplot2::xlab("")+ theme_mp_hist
    
    p1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gLine))
    p2 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(ghist))
    
    maxWidth <- grid::unit.pmax(p1$widths[2:3], p2$widths[2:3])
    
    p1$widths[2:3] <- maxWidth
    p2$widths[2:3] <- maxWidth
    
    gridExtra::grid.arrange(p1,p2,ncol=1,nrow=2,heights=c(4,1))
    
    if (interactive) {
      df <- data.frame(data.agg,freq=data.freq$freq)
      gvisSingleOptionList <- list(pointSize=8,
                                   series="[
                                   {targetAxisIndex:0, type:'line',color:'magenta',pointShape: 'square'},
                                   {targetAxisIndex:1, type:'bars',color:'yellow'}]",
                                   crosshair="{trigger:'both'}",
                                   hAxis.title=xname,
                                   theme="maximized",
                                   title=paste0("Observation analysis on ",xname, " Observed"),
                                   vAxes="{1:{format:'##.#%',maxValue:1}}",
                                   explorer="{ actions: ['dragToZoom', 'rightClickToReset'],keepInBounds: true }",
                                   chartArea="{width:'90%',height:'90%'}",
                                   height=750)
      
      plot(googleVis::gvisComboChart(df,xvar="x",yvar="y",options=gvisSingleOptionList))
      
    }
  }
  else{
    data.plot <- data.table::as.data.table(as.data.frame(cbind(x=x,y=y,w=w,by=by),stringsAsFactors=FALSE))
    data.table::setkey(data.plot,x,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(x,by),.SDcols=c("y","w")]
    
    data.agg <- as.data.frame(data.plot[,lapply(.SD,weighted.mean,w=w),by=list(x,by),.SDcols=c("y","w")],row.names=c("xvar","by","weights","observed"))
    
    data.freq <- as.data.frame(data.plot[,sum(w),by=list(x,by)][,freq:=V1/sum(V1)])
    
    #line graph
    gLine1 <- ggplot2::ggplot(data=data.agg,aes(x=x,y=y,group=factor(by),colour=factor(by)))+
      ggplot2::geom_line(size=1) + ggplot2::geom_point(size=4,fill="white")
    if("xlim" %in% opts) gLine1 <-gLine1 + xlim(xlim)
    if("ylim" %in% opts) gLine1 <-gLine1 + ylim(ylim)
    gLine1 <- gLine1+ggplot2::xlab("")+ggplot2::ylab(yname)+ ggplot2::ggtitle(paste("Observation Analysis on: ",xname," by ",byname))+
      theme_mp_line
    if(nlevels(as.factor(data.agg$x))>25) gLine1 <- gLine1 + ggplot2::theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))
    
    #histogram graph
    ghist <- ggplot2::ggplot(data=data.freq,aes(x=x,y=freq,fill=factor(by)))+ ggplot2::geom_histogram(stat="identity",binwidth=1)
    if("xlim" %in% opts) ghist <-ghist + ggplot2::xlim(xlim)
    ghist <- ghist + ggplot2::xlab("")+ ggplot2::ylab("percent (%)")+ ggplot2::scale_y_continuous(labels = percent) + 
      theme_mp_hist
    
    p1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gLine1))
    p2 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(ghist))
    
    maxWidth <- grid::unit.pmax(p1$widths[2:3], p2$widths[2:3])
    
    p1$widths[2:3] <- maxWidth
    p2$widths[2:3] <- maxWidth
    
    gridExtra::grid.arrange(p1,p2,ncol=1,nrow=2,heights=c(4,1))
  }
}

#theme for plot ggplot2 graph
theme_mp_line <- ggplot2::theme_bw() + 
  ggplot2::theme(text=ggplot2::element_text(face="bold.italic"),
                 legend.justification=c(1,1),
                 legend.position=c(1,1))


theme_mp_hist <- ggplot2::theme_bw() + 
  ggplot2::theme(text=ggplot2::element_text(face="bold.italic"),
                 axis.text.x = ggplot2::element_blank(),
                 legend.position="none") 
