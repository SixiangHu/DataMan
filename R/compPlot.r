#' compPlot
#' 
#' @description compare model predictions
#' @usage compPlot(x,act,pred,by=NULL,weights=NULL,newGroupNum=10)
#' @author Sixiang Hu
#' @importFrom data.table as.data.table data.table setkey := setnames melt
#' @importFrom plotly plot_ly add_trace layout
#' @export compPlot
#' @examples
#'  
#' compPlot(mtcars$vs,act=rnorm(nrow(mtcars)),pred = data.frame(pred1=rnorm(nrow(mtcars)),pred2=rnorm(nrow(mtcars))),by=mtcars$cyl)

compPlot <- function(x,act,pred,by=NULL,weights=NULL,newGroupNum=10){
  if (is.null(x)) stop("x provided is blank.")
  if (is.null(act)) stop("act provided is blank.")
  if (is.null(pred)) stop("pred provided is blank.")
  
  str_pred <- NULL
  num_pred <- NULL
  if (length(x) != length(act)) stop("x and act don't have the same length")
  if (is.vector(pred)){
    str_pred <- "pred"
    num_pred <- 1
    if(length(x) != length(pred)) stop("x and pred don't have the same length")
  }
  else {
    str_pred <- colnames(pred)
    if (is.null(str_pred)) str_pred <- paste("Pred",1:nrow(pred),sep=".")
    num_pred <- ncol(pred)
    if(length(x) != nrow(pred)) stop("x and pred don't have the same length")
  }
  
  if(is.null(weights)) {weights <- rep(1,length(x))}
  
  #New Group for data which has too much levels.
  if ( (is.numeric(x) || is.integer(x)) && nlevels(as.factor(x))>100 ) {
    if ( is.null(newGroupNum) ) newGroupNum <- 10
    new_band <- dmBreak(x,newGroupNum)
    x <- cut(x,new_band,include.lowest = TRUE,ordered_result = TRUE)
  }
  
  #set strings for plotting
  dp_name_str <- c("act",str_pred,"weights")
  strTitle <- paste("Fitting Analysis on:",deparse(substitute(x)))
  
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="Response", 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  ay2 <- list(side = "right", showgrid=FALSE, title="Weights",
              linecolor = "#000000")
  ax <- list(title="var", showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5")
  l <- list(bordercolor = "#000000",borderwidth=1)
  
  if(!is.null(by)){
    data.plot <- data.table::as.data.table(as.data.frame(cbind(x,by,act,pred,weights),stringsAsFactors=FALSE))
    data.table::setnames(data.plot,c("xvar","by","act",str_pred,"weights"))
    data.table::setkey(data.plot,xvar,by)

    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(xvar,by),.SDcols=dp_name_str]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=list(xvar,by),.SDcols=dp_name_str]
    data.agg2 <- data.table::melt(data.agg,id.vars = c("xvar","act","weights","by"),measure.vars =str_pred)[order(variable,by,xvar)]
    data.hist <- data.plot[,sum(w),by=list(xvar,by)][,freq:=V1/sum(V1)][order(by,xvar)]
    
    suppressWarnings(
      plotly::plot_ly(data=data.agg,x=xvar,y=act,color=paste("Observed",by,sep="-"),yaxis = "y1")%>%
        plotly::add_trace(data=data.agg2,x=xvar,y=value,color=paste("Model",variable,by,sep="-"),yaxis = "y1")%>%
        plotly::add_trace(x=xvar, y=freq, color=by, data=data.hist, type="bar",
                          marker=list(line=list(color="#606060", width=1.5)),
                          showlegend=FALSE, opacity=0.5, yaxis="y2") %>%
        plotly::layout(title=strTitle, legend=l, barmode="stack",
                       xaxis=ax, yaxis=ay1, yaxis2=ay2)
    )
  }
  else {
    data.plot <- data.table::as.data.table(as.data.frame(cbind(x,act,pred,weights),stringsAsFactors=FALSE))
    data.table::setnames(data.plot,c("xvar","act",str_pred,"weights"))
    data.table::setkey(data.plot,xvar)

    data.plot <- data.plot[,lapply(.SD,as.numeric),by=xvar,.SDcols=dp_name_str]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=xvar,.SDcols=dp_name_str]
    data.agg2 <- data.table::melt(data.agg,id.vars = c("xvar","act","weights"),measure.vars =str_pred)[order(variable,xvar)]
    data.hist <- data.plot[,sum(w),by=xvar][,freq:=V1/sum(V1)][order(xvar)]
    
    plotly::plot_ly(data=data.agg, x=xvar, y=act, color="Observed", yaxis="y1") %>%
      plotly::add_trace(data=data.agg2, x=xvar, y=value,color=variable, yaxis="y1") %>%
      plotly::add_trace(x=xvar,y=freq,data=data.hist,type="bar",showlegend=FALSE,
                        marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                        opacity=0.5,yaxis = "y2") %>%
      plotly::layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2=ay2, legend=l)
  }

}
