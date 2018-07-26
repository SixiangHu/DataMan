#' compPlot: data visualisation comparing observation and model prediction(s).
#' 
#' @description This function allows you to visualise different model predictions and 
#'     observations by certain factors.
#' @usage compPlot(x, act, pred, by = NULL, weights = NULL, exposure = NULL, 
#'    breaks = NULL, missing=TRUE, newGroupNum = 10,xlim=NULL, 
#'    xname = "x",yname="y",byname="by",legendPos=NULL) 
#' @param x a vector indicates the dependent variable that you want to visulise on (i.e. Age)
#' @param act a vector indicates the actual response variable (observation)
#' @param pred a vector or data frame that provides model predictions. 
#'     Each column must be from the same model predictions.
#' @param by Optinal.A numerical vector to specify the <by> variable
#' @param weights Optional. A numerical vector to specify the weights used for calculating weighted average of response.
#' Normally this is the figures from over/down-samping.
#' @param exposure Optional. A numerical vector to specify the exposure used for calculating weighted average of response.
#' @param breaks Optional. A vector to specify the breaks for `x`
#' @param missing logical. whether to show the `NA` as `Missing` in plot.  
#' If a `Missing` level is already existed, then `NA` will combined
#' @param newGroupNum An integer specifies number of new bands 
#' when levels of current plotting variable `x` or `by` is more than 100. 
#' @param xlim Optional. A vector provides the range of the variable e.g. xlim=c(0,100).
#' @param xname,yname,byname Optional. Characters to be shown on plot.
#' @param legendPos Optional. 2 numeric element vector specify the location of legend.
#' 
#' @author Sixiang.Hu
#' 
#' @importFrom data.table as.data.table data.table setkey := uniqueN setnames melt
#' @importFrom plotly plot_ly add_trace add_markers add_bars layout %>%
#' @importFrom checkmate testDate
#' @export
#'
#' @examples
#'  
#' compPlot(mtcars$vs,
#'          act=rnorm(nrow(mtcars)),
#'          pred = data.frame(pred1=rnorm(nrow(mtcars)),
#'                            pred2=rnorm(nrow(mtcars))),
#'          by=mtcars$am)
        
compPlot <- function(x, act, pred, by = NULL, weights = NULL, exposure = NULL, 
                     breaks = NULL,missing=TRUE, newGroupNum = 10, xlim=NULL, 
                     xname = "x",yname="y",byname="by",legendPos = NULL){
  if (is.null(x)) stop("x provided is blank.")
  if (is.null(act)) stop("act provided is blank.")
  if (is.null(pred)) stop("pred provided is blank.")
  if (length(x) != length(act)) stop("x and act don't have the same length")
  
  # range index
  if ( is.null(xlim) ) {
    ind <- 1:length(x)
  }else if ( !is.null(xlim) && !(is.numeric(x) || testDate(x)) ) {
    ind <- 1:length(x)
    warning("xlim is provided on character variable. Ignored")
  }
  else if (length(xlim)>2 || length(xlim)<2) {
    ind <- 1:length(x)
    warning("xlim provided must has 2 elements.")
  }
  else ind <- which(x>=xlim[1] && x<=xlim[2])
  
  str_pred <- character(0)
  num_pred <- numeric(0)
  
  if (is.vector(pred)) {
    str_pred <- deparse(substitute(pred))
    num_pred <- 1
    if (length(x) != length(pred)) stop("x and pred don't have the same length")
    col <- str_pred
  }else {
    str_pred <- colnames(pred)
    if (is.null(str_pred)) str_pred <- paste("Pred", 1:nrow(pred), sep = ".")
    num_pred <- ncol(pred)
    if (length(x) != nrow(pred)) stop("x and pred don't have the same length")
    col <- paste("Model",str_pred, by, sep = "-")
  }
  
  if (is.null(weights)) weights <- rep(1, length(x))
  if (is.null(exposure)) exposure <- rep(1, length(x))

  #New Group for data which has too much levels or when user specified breaks.
  if ( (is.numeric(x) & uniqueN(x)>100) | !is.null(breaks) ) {
    if(is.null(breaks)) breaks <- dmBreak(x,newGroupNum)
    x <- cut(x,breaks,include.lowest = TRUE,ordered_result = TRUE)
  }
  
  if (!is.null(by)) {
    if ( is.integer(by) & uniqueN(by[ind])>20 ) {
      new_band <- dmBreak(by,newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE,ordered_result=TRUE)
    }else if (!is.integer(by)){
      warning("'by' variable needs to be integer. Plot create will ignore 'by' variable.\n")
      by <- NULL
    }
  }

  dp_name_str <- c("act", str_pred, "weights","exposure")
  strTitle <- paste("Visual Analysis on:", xname)
  ay1 <- list(overlaying = "y2", side = "left", title = "Response", linecolor = "#000000", gridcolor = "#E5E5E5")
  ay2 <- list(side = "right", showgrid = FALSE, title = "Exposure (%)", linecolor = "#000000")
  ax <- list(title = "", showline = TRUE, linecolor = "#000000", gridcolor = "#E5E5E5")
  l <- list(bordercolor = "#000000", borderwidth = 1,orientation="h",x=legendPos[1],y=legendPos[2])
  m <- list(l=-5,r=-5,b=-5,t=-5,pad=0)
  
  if (!is.null(by)) {
    strTitle  <- paste(strTitle," by ",byname,sep="")
    
    data.plot <- data.table(x=x[ind], by = by[ind], act=act[ind], pred[ind,], 
                                        weights=weights[ind], exposure=exposure[ind])
    setnames(data.plot, c("xvar", "by", "act", str_pred, "weights","exposure"))
    setkey(data.plot, xvar, by)
    
    if(missing & class(data.plot[["xvar"]])[1]=="character"){
      set(data.plot,i=which(is.na(data.plot[["xvar"]])),j="xvar",value="Missing")
      set(data.plot,i=which(is.na(data.plot[["by"]])),j="by",value="Missing")
    }
    
    for (v_name in dp_name_str) set(data.plot,j=v_name,value=as.numeric(data.plot[[v_name]]))
    
    data.agg <- data.plot[, lapply(.SD, function(x,w,e) sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE), 
                                   w = weights,e=exposure),
                          by = list(xvar, by), .SDcols = setdiff(dp_name_str,c("weights","exposure"))]

    data.agg2 <- melt(data.agg,id.vars = c("xvar", "act", "by"),measure.vars = str_pred)[order(variable, by, xvar)]
    
    data.hist <- data.plot[, sum(exposure), by = list(xvar, by)][, freq:=round(V1/sum(V1)*100,1)][order(by, xvar)]

    plot_ly() %>%
      add_trace(y = ~act,color = ~paste("Observed",by,sep="-"),yaxis = "y1",
                        marker=list(symbol="square"),mode = 'lines+markers', type = 'scatter') %>%
      add_trace(data = data.agg2,x = ~xvar, y = ~value,
                        color= ~paste("Model",variable,by,sep="-"),yaxis = "y1",
                        marker=list(symbol="triangle-up"),mode = 'lines+markers', type = 'scatter') %>% 
      add_bars(x = ~xvar, y = ~freq, color = ~by, data = data.hist,  
                      marker = list(line = list(color = "#606060", width = 1.5)), 
                       showlegend = FALSE, opacity = 0.5, yaxis = "y2") %>% 
      layout(title = strTitle, legend = l, barmode = "stack", 
                     xaxis = ax, yaxis = ay1, yaxis2 = c(ay2,list(range=c(0,min(max(data.hist$freq)*2.5,100)))),
                     margin=m)
  }
  else {
    data.plot <- data.table(x=x[ind], act=act[ind], pred[ind,], 
                                        weights=weights[ind], exposure=exposure[ind])
    setnames(data.plot, c("xvar", "act", str_pred, "weights","exposure"))
    setkey(data.plot, xvar)
    
    if(missing & class(data.plot[["xvar"]])[1]=="character"){
      set(data.plot,i=which(is.na(data.plot[["xvar"]])),j="xvar",value="Missing")
    }
    
    for (v_name in dp_name_str) set(data.plot,j=v_name,value=as.numeric(data.plot[[v_name]]))
    
    data.agg <- data.plot[, lapply(.SD, function(x,w,e) sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE), 
                                   w = weights,e=exposure), 
                          by = xvar, .SDcols = setdiff(dp_name_str,c("weights","exposure"))]

    data.agg2 <- melt(data.agg, id.vars = c("xvar","act"), measure.vars = str_pred)[order(variable,xvar)]
    
    data.hist <- data.plot[, sum(exposure), by = xvar][, freq:=round(V1/sum(V1)*100,1)][order(xvar)]

    plot_ly(data = data.agg,x = ~xvar) %>%
         add_trace(y = ~act,line=list(color="#CC3399"),name="Observed",yaxis = "y1",
                           marker=list(symbol="square"),mode = 'lines+markers', type = 'scatter') %>%
         add_trace(data = data.agg2,x = ~xvar, y = ~value,color=~variable,yaxis = "y1",
                           marker=list(symbol="triangle-up"),mode = 'lines+markers', type = 'scatter') %>% 
         add_bars(x = ~xvar, y = ~freq, data = data.hist, showlegend = FALSE, 
                          marker = list(color = "#ffed00", line = list(color = "#000000", width = 1.5)), 
                          opacity = 0.5, yaxis = "y2") %>% 
         layout(title = strTitle, xaxis = ax, yaxis = ay1, 
                        yaxis2 = c(ay2,list(range=c(0,min(max(data.hist$freq)*2.5,100)))), legend = l
                        ,margin=m)
  }
}
