#' ROC (AUC) plot
#'
#' @description 
#' With provided different model predictions, different lift curves will be 
#' plot to compare model improvement. 
#' @usage rocPlot(data,actual)
#' @param data A vector or `data.frame` includes columns 
#' that each column standards predictions from a model.
#' @param actual Numerical vector for actual or true result.
#' @author Sixiang Hu
#' @importFrom ROCR prediction performance
#' @importFrom plotly plot_ly add_trace layout %>%
#' @export rocPlot
#' @examples
#' set.seed(1L)
#' a <- sample(c(0,1),10000,replace=TRUE)
#' b <- runif(10000)
#' c <- runif(10000)
#' rocPlot(cbind(b,c),a)
 
rocPlot <- function(data,actual){
  if ("matrix" %in% class(data) ) data <- as.data.frame(data)
  
  if ( sum(c("data.frame","data.table") %in% class(data))==0) iVar <- 1
  else iVar <- dim(data)[2]
  
  auc <- data.frame(x=NULL,y=NULL,ModelName=NULL)
  
  for (i in 1:iVar){
    
    if(iVar!=1) pred <- prediction(data[[i]],actual)
    else pred <- prediction(data,actual)
    
    pref <- performance(pred,measure = "tpr", x.measure = "fpr")
    pref2 <- performance(pred,measure = "auc")
    num_auc <- signif(pref2@y.values[[1]],3)
 
    strName <- names(data)[i]
    if (is.null(strName)) strName <- paste("Pred",i," (AUC: ",num_auc,")",sep="")
    else strName <- paste(strName," (AUC:",num_auc,")",sep="")
    
    tmp <- data.frame(x=pref@x.values,y=pref@y.values,ModelName = strName)
    names(tmp) <- c("x","y","ModelName")
    
    auc <- rbind(auc, tmp)
  }
  
  #set axis
  ay1 <- list(overlaying = "y2", side = "left", title="True Positive Rate", 
              linecolor = "#000000", gridcolor = "#E5E5E5")

  ax <- list(title="False Positive Rate", showline=TRUE, linecolor = "#000000",
             gridcolor = "#E5E5E5")

  l <- list(bordercolor = "#000000",borderwidth=1,orientation="h")
  
  plot_ly(data = auc) %>%
    add_trace(x=~x,y=~y,color=~ModelName,mode = 'lines', type = 'scatter') %>%
    add_trace(x=c(0,1),y=c(0,1),line=list(color="red"),name="Diagnal Line",
                      mode = 'lines', type = 'scatter') %>%
    layout(title="ROC (AUC) Curve",xaxis=ax,yaxis = ay1,legend=l)
}

#global variable
globalVariables(c("x","y","ModelName"))