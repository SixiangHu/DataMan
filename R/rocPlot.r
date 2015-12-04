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
#' @importFrom dplyr %>%
#' @importFrom ROCR prediction performance
#' @importFrom rbokeh figure ly_lines ly_points ly_abline
#' @export rocPlot
#' @examples
#' 
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
    
    if(iVar!=1) pred <- ROCR::prediction(data[[i]],actual)
    else pred <- ROCR::prediction(data,actual)
    
    pref <- ROCR::performance(pred,measure = "tpr", x.measure = "fpr")
    pref2 <- ROCR::performance(pred,measure = "auc")
    num_auc <- signif(pref2@y.values[[1]],3)
    
    strName <- names(data)[i]
    if (is.null(strName)) strName <- paste("Pred",i," (AUC: ",num_auc,")",sep="")
    else strName <- paste(strName," (AUC:",num_auc,")",sep="")
    
    tmp <- data.frame(x=pref@x.values,y=pref@y.values,ModelName = strName)
    names(tmp) <- c("x","y","ModelName")
    
    auc <- rbind(auc, tmp)
  }

  rbokeh::figure(title="ROC (AUC) Curve", tools=.tools,
                 width=700, height=700,
                 xlab="False Positive Rate",ylab="True Positive Rate",
                 xlim=c(0,1),ylim=c(0,1),
                 legend_location = "bottom_right") %>%
    rbokeh::ly_lines(x,y,data = auc,color=ModelName) %>%
    rbokeh::ly_abline(a=0,b=1,color="red")
}

#global variable
globalVariables(c("x","y","ModelName"))