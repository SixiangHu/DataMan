#' Lift Plot
#'
#' @description 
#' With provided different model predictions, different lift curves will be 
#' plot to compare model improvement. 
#' @usage liftPlot(act,pred1,pred2,weight=NULL,exposure=NULL,breaks=seq(0,2,0.05),showas=NULL)
#' @param act A numeric vector to give actual for different.
#' @param pred1 A numeric vector to give actual for different.
#' @param pred2 A numeric vector to give actual for different.
#' @param weight A numeric vector to give weights for different predictions.
#' @param exposure A numeric vector to give exposure for different predictions.
#' @param breaks A sequence of the numericals defining breaks that differences are split into.
#' @param showas A string vectors to give names for each model on the lift curve.
#' @author Sixiang Hu
#' @importFrom data.table set setkey data.table
#' @importFrom plotly plot_ly add_trace add_bars layout %>%
#' @export liftPlot
#' @examples
#' 
#' glm1 <- glm(mpg~cyl,data=mtcars,family=Gamma(log))
#' glm2 <- glm(mpg~cyl+vs,data=mtcars,family=Gamma(log))
#' pred1 <- predict(glm1,mtcars)
#' pred2 <- predict(glm2,mtcars)
#' liftPlot(mtcars$mpg,pred1,pred2,breaks=seq(0.9,1.1,0.01))

liftPlot <- function(act,pred1,pred2,weight=NULL,exposure=NULL,breaks=seq(0,2,0.05),showas=NULL){

  if((length(act)!=length(pred1)) || (length(act)!=length(pred2)) || (length(pred1)!=length(pred2)))
    stop("Provided act, pred1, and pred2 have different length.")
  
  if(length(act)<length(breaks)*10) 
      warning("Number of obs per bucket is less than 10, which may not give a reliable weighted mean.") 
  
  if (is.null(weight)) weight <- rep(1,length(act))
  if (is.null(exposure)) exposure <- rep(1,length(act))
  
  if (is.null(showas) )  showas <- c("pred1","pred2") 
  else if (!is.null(showas) && length(showas)<2 ) stop("Only one new name has been provided.")
  
  dt <- data.table::data.table(act,pred1,pred2,weight,exposure)
  dt[,ratio:=pred1/pred2]
  dt[,breaks:=cut(ratio,breaks,include.lowest = FALSE,ordered_result = TRUE)]
  data.table::setkey(dt,breaks)
  dt_agg  <- dt[,c(obs=sum(weight*exposure,na.rm=TRUE),
                   lapply(.SD,function(x,w,e)sum(x*w,na.rm = TRUE)/sum(e*w,na.rm = TRUE),e=exposure,w=weight),
                   weight=sum(weight),
                   exposure=sum(exposure)
                   ),
                by=breaks,.SDcols=c("act","pred1","pred2")]
  dt_full <- data.table::data.table(breaks=cut(breaks,breaks,include.lowest = FALSE,ordered_result = TRUE))
  data.table::setkey(dt_full,breaks)
  dt_final <- dt_agg[dt_full,on="breaks"]
  dt_final[is.na(dt_final)] <- 0
  
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="Response", 
              linecolor = "#000000", gridcolor = "#E5E5E5")
  
  ay2 <- list(side = "right", showgrid=FALSE, title="Exposure (%)",
              linecolor = "#000000")
  
  ax <- list(title=paste0(showas[1],"/",showas[2]), showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5",type = "category",
             categoryorder = "category ascending")
  
  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  m <- list(l=-5,r=-5,b=-5,t=-5,pad=0)
  
  #Plotting
  strTitle <- paste("Impact Analysis comparing: ",showas[1]," and ",showas[2])
  
  plotly::plot_ly(data = dt_final,x=~breaks,y=~act,name="Observed")%>%
    plotly::add_trace(line=list(color="#CC3399"),yaxis="y1",
                      marker=list(color="#CC3399",symbol="square",size=10),mode = 'lines+markers', type = 'scatter') %>%
    plotly::add_trace(x=~breaks,y=~pred1, line=list(color="#336633",shape = "linear"),mode = 'lines+markers', type = 'scatter',
                      marker=list(symbol="triangle-up",size=10), name=showas[1],yaxis = "y1") %>%
    plotly::add_trace(x=~breaks,y=~pred2, line=list(color="#33CC33",shape = "linear"),mode="lines+markers", type = 'scatter',
                      marker=list(symbol="triangle-down",size=10), name=showas[2],yaxis = "y1") %>%
    plotly::add_bars(x=~breaks,y=~exposure,showlegend=FALSE,
                     marker=list(color="#99CCFF",line=list(color="#606060",width=1.5)),
                     opacity=0.5,yaxis = "y2") %>%
    plotly::layout(title = strTitle, xaxis=ax,yaxis = ay1,
                   yaxis2 = c(ay2,list(range=c(0,min(max(dt_final$exposure)*2.5,100)))),
                   legend=l,margin=m)
}

#global variable
globalVariables(c("ratio"))