#' compPlot: data visualisation comparing observation and model prediction(s).
#' 
#' @description This function allows you to visualise different model predictions and observations by certain factors.
#' @usage compPlot(x, act, pred, by = NULL, weights = NULL, exposure = NULL, 
#' breaks = NULL, missing=TRUE, newGroupNum = 10,xname = "x",yname="y",byname="by") 
#' @param x a vector indicates the dependent variable that you want to visulise on (i.e. Age)
#' @param act a vector indicates the actual response variable (observation)
#' @param pred a vector or data frame that provides model predictions. Each column must be from the same model predictions.
#' @param by Optinal.A numerical vector to specify the <by> variable
#' @param weights Optional. A numerical vector to specify the weights used for calculating weighted average of response.
#' normally this is the figures from over/down-samping.
#' @param exposure Optional. A numerical vector to specify the exposure used for calculating weighted average of response.
#' @param breaks Optional. A vector to specify the breaks for `x`
#' @param missing logical. whether to show the `NA` as `Missing` in plot.  
#' If a `Missing` level is already existed, then `NA` will combined
#' @param newGroupNum An integer specifies number of new bands 
#' when levels of current plotting variable `x` or `by` is more than 100. 
#' @param xname,yname,byname Optional. Characters to be shown on plot.
#' 
#' @author Sixiang.Hu
#' 
#' @importFrom data.table as.data.table data.table setkey := uniqueN
#' @importFrom plotly plot_ly add_trace add_markers add_bars layout %>%
#' 
#' @export
#'
#' @examples
#'  
#' compPlot(mtcars$vs,
#'          act=rnorm(nrow(mtcars)),
#'          pred = data.frame(pred1=rnorm(nrow(mtcars)),
#'                            pred2=rnorm(nrow(mtcars))),
#'          by=mtcars$cyl)
        
compPlot <- function(x, act, pred, by = NULL, weights = NULL, exposure = NULL, breaks = NULL,
                     missing=TRUE, newGroupNum = 10,xname = "x",yname="y",byname="by"){
  if (is.null(x)) stop("x provided is blank.")
  if (is.null(act)) stop("act provided is blank.")
  if (is.null(pred)) stop("pred provided is blank.")
  if (length(x) != length(act)) stop("x and act don't have the same length")
  
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

  #New Group for data which has too much levels.
  if ( (is.numeric(x) || is.integer(x)) && data.table::uniqueN(x)>100 ) {
    if(is.null(breaks)) breaks <- dmBreak(x,newGroupNum)
    x <- cut(x,breaks,include.lowest = TRUE,ordered_result = TRUE)
  }
  
  if (!is.null(by)) {
    if ( is.numeric(by) & data.table::uniqueN(by)>20 ) {
      new_band <- dmBreak(by,newGroupNum)
      by <- cut(by,new_band,include.lowest = TRUE,ordered_result=TRUE)
    }else if (is.numeric(by)) by <- as.factor(by)
  }

  dp_name_str <- c("act", str_pred, "weights","exposure")
  strTitle <- paste("Visual Analysis on:", xname)
  ay1 <- list(overlaying = "y2", side = "left", title = "Response", linecolor = "#000000", gridcolor = "#E5E5E5")
  ay2 <- list(side = "right", showgrid = FALSE, title = "Exposure (%)", linecolor = "#000000")
  ax <- list(title = "", showline = TRUE, linecolor = "#000000", gridcolor = "#E5E5E5")
  l <- list(bordercolor = "#000000", borderwidth = 1,orientation="h")
  m <- list(l=-5,r=-5,b=-5,t=-5,pad=0)
  
  if (!is.null(by)) {
    strTitle  <- paste(strTitle," by ",byname,sep="")
    
    data.plot <- data.table::data.table(x, by, act, pred, weights, exposure)
    data.table::setnames(data.plot, c("xvar", "by", "act", str_pred, "weights","exposure"))
    data.table::setkey(data.plot, xvar, by)
    
    if(missing & class(data.plot[["xvar"]])[1]=="character"){
      set(data.plot,i=which(is.na(data.plot[["xvar"]])),j="xvar",value="Missing")
      set(data.plot,i=which(is.na(data.plot[["by"]])),j="by",value="Missing")
    }
    
    for (v_name in dp_name_str) set(data.plot,j=v_name,value=as.numeric(data.plot[[v_name]]))
    
    data.agg <- data.plot[, lapply(.SD, function(x,w,e) sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE), w = weights,e=exposure),
                          by = list(xvar, by), .SDcols = setdiff(dp_name_str,c("weights","exposure"))]

    data.agg2 <- data.table::melt(data.agg,id.vars = c("xvar", "act", "by"),measure.vars = str_pred)[order(variable, by, xvar)]
    
    data.hist <- data.plot[, sum(exposure), by = list(xvar, by)][, freq:=round(V1/sum(V1)*100,1)][order(by, xvar)]

    plotly::plot_ly() %>%
      plotly::add_trace(y = ~act,color = ~paste("Observed",by,sep="-"),yaxis = "y1",
                        marker=list(symbol="square"),mode = 'lines+markers', type = 'scatter') %>%
      plotly::add_trace(data = data.agg2,x = ~xvar, y = ~value,
                        color= ~paste("Model",variable,by,sep="-"),yaxis = "y1",
                        marker=list(symbol="triangle-up"),mode = 'lines+markers', type = 'scatter') %>% 
      plotly::add_bars(x = ~xvar, y = ~freq, color = ~by, data = data.hist,  
                      marker = list(line = list(color = "#606060", width = 1.5)), 
                       showlegend = FALSE, opacity = 0.5, yaxis = "y2") %>% 
      plotly::layout(title = strTitle, legend = l, barmode = "stack", 
                     xaxis = ax, yaxis = ay1, yaxis2 = c(ay2,list(range=c(0,min(max(data.hist$freq)*2.5,100)))),
                     margin=m)
  }
  else {
    data.plot <- data.table::data.table(x, act, pred, weights, exposure)
    data.table::setnames(data.plot, c("xvar", "act", str_pred, "weights","exposure"))
    data.table::setkey(data.plot, xvar)
    
    if(missing & class(data.plot[["xvar"]])[1]=="character"){
      set(data.plot,i=which(is.na(data.plot[["xvar"]])),j="xvar",value="Missing")
    }
    
    for (v_name in dp_name_str) set(data.plot,j=v_name,value=as.numeric(data.plot[[v_name]]))
    
    data.agg <- data.plot[, lapply(.SD, function(x,w,e) sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE), w = weights,e=exposure), by = xvar, .SDcols = setdiff(dp_name_str,c("weights","exposure"))]

    data.agg2 <- data.table::melt(data.agg, id.vars = c("xvar","act"), measure.vars = str_pred)[order(variable,xvar)]
    
    data.hist <- data.plot[, sum(exposure), by = xvar][, freq:=round(V1/sum(V1)*100,1)][order(xvar)]

    plotly::plot_ly(data = data.agg,x = ~xvar) %>%
         plotly::add_trace(y = ~act,line=list(color="#CC3399"),name="Observed",yaxis = "y1",
                           marker=list(symbol="square"),mode = 'lines+markers', type = 'scatter') %>%
         plotly::add_trace(data = data.agg2,x = ~xvar, y = ~value,color=~variable,yaxis = "y1",
                           marker=list(symbol="triangle-up"),mode = 'lines+markers', type = 'scatter') %>% 
         plotly::add_bars(x = ~xvar, y = ~freq, data = data.hist, showlegend = FALSE, 
                          marker = list(color = "#ffed00", line = list(color = "#000000", width = 1.5)), 
                          opacity = 0.5, yaxis = "y2") %>% 
         plotly::layout(title = strTitle, xaxis = ax, yaxis = ay1, 
                        yaxis2 = c(ay2,list(range=c(0,min(max(data.hist$freq)*2.5,100)))), legend = l
                        ,margin=m)
  }
}
