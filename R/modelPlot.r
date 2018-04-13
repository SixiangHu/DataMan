#' modelPlot
#'
#' @description This function allows you visulise the model fitting by 
#' comparing observation, fitted and mean fitted in one plot.
#' @usage modelPlot(model,xvar,act,wvar=NULL,
#' by=NULL,dataset=NULL,newGroupNum=10,predFUN=predict,...)
#' @param model a model object. It can be any model object that has a 
#' corresponding prediction function. 
#' @param xvar either an integer to specify the position of the dependent variable 
#' in the data frame, or a character string to indicate the dependent variable name.
#' @param act either an integer to specify the position of the dependent variable 
#' in the data frame, or a character string to indicate the response variable
#' @param wvar Optional. A numerical vector, integer, or variable name to 
#' specify the weights used for model visualisation.
#' @param by Optinal. A character string indicates the variable name you want to plot the fit by.
#' @param dataset A data frame.
#' @param newGroupNum Optional. An integer specifies number of new bands 
#' when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @param predFUN prediction function to the corresponding model. 
#' @param ... optional augments to predFUN. For example, 
#' glm model needs `type="response"` to predict actual values.
#' @details 
#' For the purpose of comparing across different types of statistical or
#' machine learning models, this plot function gives a general method to 
#' put actual (observation), model predictions (fitted), and marginal effect
#' (fitted mean) on the same graph.
#' 
#' Currently, this function is not restricted to any package or models as long
#' as it has a prediction function for the model. 
#' 
#' @author Sixiang Hu
#' @importFrom data.table data.table setkey := uniqueN
#' @importFrom plotly plot_ly add_lines add_bars layout %>%
#' @importFrom stats predict
#' @seealso \code{\link{glm}}
#' @export modelPlot
#' @examples
#' 
#' ## glm example
#' 
#' glm1 <- glm(formula = mpg ~ cyl + hp, family = Gamma(log), data = mtcars, weights = wt)
#' modelPlot(glm1,"cyl","mpg","wt",dataset=mtcars,type="response")

modelPlot <- function(model,
                      xvar,
                      act,
                      wvar=NULL,
                      by=NULL,
                      dataset=NULL,
                      newGroupNum=10,
                      predFUN = predict,...){
  
  predFUN <- match.fun(predFUN)
  
  if ( !exists(deparse(substitute(model))) ) stop("Model provided is blank.")

  #data source
  if (.isDFnull(dataset)) stop("modelPlot: Cannot found any proper data set.")
  
  #xvar
  tmp <- .VarPosition(dataset,xvar)
  xvar <- tmp$name
  
  #weights
  if (is.null(wvar)) weights <- rep(1,nrow(dataset))
  else if (length(wvar)>1 && length(wvar)!=nrow(dataset)) 
    stop("weights provided must have the same length as dataset.")
  else if(length(wvar)==nrow(dataset)){weights <- wvar}
  else {
    tmp <- .VarPosition(dataset,wvar)
    weights <- dataset[[tmp$posi]]
  }
  
  #by
  if (!is.null(by)){
    if (length(by)>1 && length(by)!=nrow(dataset)) 
      stop("by provided must have the same length as dataset.")
    else if(length(by)==nrow(dataset)){by_val<-by}
    else {
      tmp <- .VarPosition(dataset,by)
      by_val <- dataset[[tmp$posi]]
      if ( (is.numeric(by_val) || is.integer(by_val)) && data.table::uniqueN(by_val)>100 ) {
        new_band <- dmBreak(by_val,newGroupNum)
        by_val <- cut(by_val,new_band,include.lowest = TRUE,ordered_result=TRUE)
      }
      if (is.numeric(by_val)) by <- as.factor(by_val)
    }
  }

  #observed
  if (length(act)>1 && length(act)!=nrow(dataset)) 
    stop("actual value provided must have the same length as dataset.")
  else if(length(act)==nrow(dataset)){observed <- act}
  else {
    tmp <- .VarPosition(dataset,act)
    observed <- dataset[[tmp$posi]]
  }

  #fitted.values
  fitted <- as.numeric(predFUN(model,dataset,...))

  #Calculate mean data set for mean_fit line
  MeanData <- .ModeData(dataset,weights)
  MeanData[,xvar] <- dataset[,xvar]
  
  #fitted mean
  fitted_mean <- as.numeric(predFUN(model,MeanData,...))
  
  #Plotting
  strTitle <- paste("Fitting Analysis on: ",xvar)
  
  #New Group for data which has too much levels.
  if ( (is.numeric(dataset[,xvar]) || is.integer(dataset[,xvar])) && data.table::uniqueN(dataset[,xvar])>100 ) {
    new_band <- dmBreak(dataset[,xvar],newGroupNum)
    dataset[,xvar] <- cut(dataset[,xvar],new_band,include.lowest = TRUE,ordered_result=TRUE)
  }
  
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="Response", 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  
  ay2 <- list(side = "right", showgrid=FALSE, title="Weights (%)",
              linecolor = "#000000")
  
  if(is.character(dataset[,xvar])) {
    ax <- list(title=xvar, showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5",type = "category",
               categoryorder = "category ascending")
  }
  else{
    ax <- list(title=xvar, showline=TRUE, linecolor = "#000000",
               gridcolor = "#E5E5E5")
  }
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  
  if (!is.null(by)) {
    strTitle <- paste(strTitle," by ",by,sep="")

    data.plot <- data.table::data.table(xvar=dataset[,xvar],by=by_val,fitted,observed,weights)
    data.table::setkey(data.plot,xvar,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(xvar,by),.SDcols=c("fitted","observed","weights")]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=list(xvar,by),.SDcols=c("fitted","observed","weights")]
    data.hist <- data.plot[,sum(weights),by=list(xvar,by)][,freq:=round(V1/sum(V1)*100)][order(xvar,by)]
    
    suppressMessages(
      plotly::plot_ly(data = data.agg,x=~xvar,y=~observed,color=~paste0("Observed: ",by)) %>%
        plotly::add_lines(yaxis = "y1")%>%
        plotly::add_lines(x=~xvar,y=~fitted,color=~paste0("Fitted: ",by),
                          line=list(dash="dot"),yaxis = "y1") %>%
        plotly::add_lines(x=~xvar,y=~fitted_mean,color=~paste("Fitted Mean: ",by),
                          line=list(dash="dash"),yaxis = "y1") %>%
        plotly::add_bars(x=~xvar,y=~freq,color=~by,data=data.hist,
                          marker=list(line=list(color="#606060",width=1.5)),
                          showlegend=FALSE, opacity=0.5,yaxis = "y2") %>%
        plotly::layout(title = strTitle,legend=l,barmode = "stack",
                       xaxis = ax,yaxis = ay1,yaxis2 = ay2)
      )
  }
  else {
    #use data.table to speed up
    data.plot <-data.table::data.table(xvar=dataset[,xvar],fitted,fitted_mean,observed,weights)
    data.table::setkey(data.plot,xvar)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")]
    data.hist <- data.plot[,sum(weights),by=xvar][,freq:=round(V1/sum(V1)*100)][order(xvar)]
  
    suppressWarnings(
    plotly::plot_ly(data = data.agg,x=~xvar,y=~observed,name="Observed")%>%
      plotly::add_lines(line=list(color="#CC3399"),yaxis="y1",mode="markers",
                        marker=list(color="#CC3399",symbol="square",size=10)) %>%
      plotly::add_lines(x=~xvar,y=~fitted, line=list(color="#336633",shape = "linear"),mode="lines+markers",
                        marker=list(symbol="triangle-up",size=10), name="Fitted",yaxis = "y1") %>%
      plotly::add_lines(x=~xvar,y=~fitted_mean, line=list(color="#33CC33",shape = "linear"),mode="lines+markers",
                        marker=list(symbol="triangle-down",size=10), name="Fitted Mean",yaxis = "y1") %>%
      plotly::add_bars(x=~xvar,y=~freq,data=data.hist,showlegend=FALSE,
                        marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                        opacity=0.5,yaxis = "y2") %>%
      plotly::layout(title = strTitle, xaxis=ax,yaxis = ay1,yaxis2 = ay2,legend=l)
    )
  }
}

#global variable
globalVariables(c(".SD"))