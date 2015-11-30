#' Model Lift Plot
#'
#' @description 
#' With provided different model predictions, different lift curves will be 
#' plot to compare model improvement. 
#' @usage liftPlot(data,weight=NULL,bucket=20,showas=NULL)
#' @param data A `data.frame` includes columns which are model predictions.
#' @param weight A numeric vector to give weights for different predictions.
#' @param bucket An integer to specify the number of groups that the total predictions are split into.
#' @param showas A string vector to give names for each predictions on the lift curve.
#' @author Sixiang Hu
#' @importFrom dplyr group_by summarise %>% 
#' @export liftPlot
#' @examples

liftPlot <- function(data,weight=NULL,bucket=20,showas=NULL){
  newNameFlag <- TRUE
  
  if(dim(data)[1]<bucket) stop("Default or given bucket number is more than data obs.") 
  if(dim(data)[1]<bucket*10) warning("Number of obs per bucket is less than 10, 
                                     which may not give a reliable weighted mean.") 
  
  if (is.null(weight)) weight <- rep(1,dim(data)[1])
  if (!is.null(showas) && length(showas) != dim(data)[2]) stop("Names provided in `showas` has a different length with column provided.") 
  else if (is.null(showas)) newNameFlag <- FALSE
  
  P <- data.frame(Group=NULL,Pred = NULL)
  
  for(i in 1:dim(data)[2]){
    temp <- liftGroup(data[,i],weight,bucket)
    if (newNameFlag) temp$ModelNames <- showas[i]
    else temp$ModelNames <- names(data)[i]
    
    P <- rbind(P,temp)
  }
  
  P <- data.frame(P)
  
  ggplot(P,aes(x=Group,y=Pred,group=ModelNames,colour=ModelNames)) +
    geom_line(size=1.5)+geom_point()+
    theme_bw()+
    theme(legend.justification=c(1,0), legend.position=c(1,0))+
    xlab("Exposure Groups")+ylab("Weighted Predictions")
}

liftGroup <- function(pred,weight=NULL,bucket){
  if (is.null(weight)) weight <- rep(1,length(pred))
  
  df <- data.frame(cbind(pred,weight))
  df <- df[order(df[,1]),]
  df$group <- floor((1:length(pred))/ ceiling(length(pred)/bucket))+1
  
  df_plot <-df %>% 
    group_by(group) %>% 
    summarise(wmean = weighted.mean(pred, weight))
  
  df_plot <- data.frame(df_plot)
  names(df_plot) <- c("Group","Pred")
  df_plot
}

1010