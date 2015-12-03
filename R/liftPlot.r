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
#' 
#' glm1 <- glm(mpg~cyl,data=mtcars,family=Gamma(log))
#' glm2 <- glm(mpg~cyl+vs,data=mtcars,family=Gamma(log))
#' pred1 <- predict(glm1,mtcars)
#' pred2 <- predict(glm2,mtcars)
#' data <- cbind(pred1,pred2)
#' liftPlot(data)

liftPlot <- function(data,weight=NULL,bucket=20,showas=NULL){
  newNameFlag <- TRUE
  
  if(dim(data)[1]<bucket) stop("Default or given bucket number is more than data obs.") 
  if(dim(data)[1]<bucket*10) warning("Number of obs per bucket is less than 10, which may not give a reliable weighted mean.") 

  if (is.null(weight)) weight <- rep(1,dim(data)[1])
  if (!is.null(showas) && length(showas) != dim(data)[2]) stop("Names provided in `showas` has a different length with column provided.") 
  else if (is.null(showas)) newNameFlag <- FALSE
  
  P <- data.frame(Group=NULL,Pred = NULL,ModelNames=NULL)
  
  for(i in 1:dim(data)[2]){
    temp <- as.data.frame(.liftGroup(data[,i],weight,bucket))

    if (newNameFlag) temp$ModelNames <- showas[i]
    else if (!is.null(names(data)[i])) temp$ModelNames <- names(data)[i]
    else temp$ModelNames <- paste("Mode; ",i,sep="")

    P <- rbind(P,temp)
  }

  tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "resize", "reset")
  
  p1 <- figure(tools = tools,height=400) %>%
    ly_lines(Group, Pred, data = P, color = ModelNames)
  
  p2 <- figure(tools = tools,height=200) %>%
    ly_hist(Group,data=P,breaks=bucket)
  
  grid_plot(list(p1, p2), same_axes = TRUE,
            nrow=2,ncol=1,byrow=TRUE,width=800)
  
}

.liftGroup <- function(pred,weight=NULL,bucket){

  df <- data.frame(cbind(pred,weight))
  df <- df[order(df[,1]),]
  df$group <- floor((1:length(pred))/ ceiling(length(pred)/bucket))+1
  
  df_plot <-df %>% 
    group_by(group) %>% 
    summarise(wmean = weighted.mean(pred, weight))
                                                                                                                                                                                                       
  names(df_plot) <- c("Group","Pred")
  df_plot
}
