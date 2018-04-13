#' compPlot
#' 
#' @description compare model predictions
#' @usage compPlot(x,act,pred,by=NULL,weights=NULL,newGroupNum=10)
#' @param x a vector indicates the dependent variable
#' @param act  a vector indicates the response variable
#' @param pred a vector or data.frame indicates the model predictions 
#' that each column is a prediction from one model.
#' @param weights Optional. A numerical vector to specify the weights used for model visualisation.
#' @param by Optinal. A vector indicates a `by` factor.
#' @param newGroupNum Optional. An integer specifies number of new bands 
#' when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @author Sixiang Hu
#' @importFrom data.table as.data.table data.table setkey := setnames melt
#' @importFrom plotly plot_ly add_trace layout
#' @export compPlot
#' @examples
#'  
#' compPlot(mtcars$vs,
#'          act=rnorm(nrow(mtcars)),
#'          pred = data.frame(pred1=rnorm(nrow(mtcars)),
#'                            pred2=rnorm(nrow(mtcars))),
#'          by=mtcars$cyl)
#'          
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
  if ( (is.numeric(x) || is.integer(x)) && data.table::uniqueN(x)>100 ) {
    new_band <- dmBreak(x,newGroupNum)
    x <- cut(x,new_band,include.lowest = TRUE,ordered_result = TRUE)
  }
  
  #New Group for byvar if it has too many levels.
  if (!is.null(by)) {
    if ((is.numeric(by) || is.integer(by)) && data.table::uniqueN(by)>20 ) {
      new_band <- dmBreak(by,newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE,ordered_result=TRUE)
    }
    if (is.numeric(by)) by <- as.factor(by)
  }
  
  #set strings for plotting
  dp_name_str <- c("act",str_pred,"weights")
  strTitle <- paste("Fitting Analysis on:",deparse(substitute(x)))
  
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="Response", 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  ay2 <- list(side = "right", showgrid=FALSE, title="Weights (%)",
              linecolor = "#000000")
  
  if(is.character(x)) {
    ax <- list(title=deparse(substitute(x)), showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5",type = "category",
               categoryorder = "category ascending")
  }
  else{
    ax <- list(title=deparse(substitute(x)), showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5")
  }
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  
  if(!is.null(by)){
    data.plot <- data.table::data.table(x,by,act,pred,weights)
    data.table::setnames(data.plot,c("xvar","by","act",str_pred,"weights"))
    data.table::setkey(data.plot,xvar,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(xvar,by),.SDcols=dp_name_str]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=list(xvar,by),.SDcols=dp_name_str]
    data.agg2 <- data.table::melt(data.agg,id.vars = c("xvar","act","weights","by"),measure.vars =str_pred)[order(variable,by,xvar)]
    data.hist <- data.plot[,sum(weights),by=list(xvar,by)][,freq:=round(V1/sum(V1)*100)][order(by,xvar)]
    
    
    resplot <- plotly::plot_ly(data=data.agg,x=~xvar,y=~act,color=~paste0("Observed: ",by)) %>%
      plotly::add_lines(yaxis = "y1") %>%
      plotly::add_lines(data=data.agg2,x=~xvar,y=~value,color=~paste0("Modelled: ", by),colors="Set1",yaxis = "y1")%>%
      plotly::add_bars(x=~xvar, y=~freq, color=~by, data=data.hist,
                       marker=list(line=list(color="#606060", width=1.5)),
                       showlegend=FALSE, opacity=0.5, yaxis="y2") %>%
      plotly::layout(title=strTitle, legend=l, barmode="stack",
                     xaxis=ax, yaxis=ay1, yaxis2=ay2)
    suppressWarnings(print(resplot))
  }
  else {
    data.plot <- data.table::data.table(x,act,pred,weights)
    data.table::setnames(data.plot,c("xvar","act",str_pred,"weights"))
    data.table::setkey(data.plot,xvar)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=xvar,.SDcols=dp_name_str]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=xvar,.SDcols=dp_name_str]
    data.agg2 <- data.table::melt(data.agg,id.vars = c("xvar","act","weights"),measure.vars =str_pred)[order(variable,xvar)]
    data.hist <- data.plot[,sum(weights),by=xvar][,freq:=round(V1/sum(V1)*100)][order(xvar)]

    resplot <- plotly::plot_ly(data=data.agg, x=~xvar, y=~act, name="Observed") %>%
      plotly::add_lines(line=list(color="#CC3399"),yaxis="y1") %>%
      plotly::add_markers(marker=list(color="#CC3399",symbol="square",size=10),showlegend=FALSE) %>%
      plotly::add_lines(data=data.agg2, x=~xvar, y=~value,color=~variable, yaxis="y1") %>%
      plotly::add_bars(x=~xvar,y=~freq,data=data.hist,showlegend=FALSE,
                       marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                       opacity=0.5,yaxis = "y2") %>%
      plotly::layout(title=strTitle, xaxis=ax, yaxis=ay1, yaxis2=ay2, legend=l)
    suppressWarnings(print(resplot))
  }
  
}