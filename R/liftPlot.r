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
#' @importFrom rbokeh figure ly_lines ly_points ly_hist grid_plot 
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

  if(dim(data)[1]<bucket*10) 
    warning("Number of obs per bucket is less than 10, which may not give a reliable weighted mean.") 

  if (is.null(weight)) weight <- rep(1,dim(data)[1])
  
  if (!is.null(showas) && length(showas) != dim(data)[2]) 
    stop("Names provided in `showas` has a different length with column provided.") 
  else if (is.null(showas)) newNameFlag <- FALSE
  
  P <- data.frame(Group=NULL,Pred = NULL,ModelNames=NULL)
  
  for(i in 1:dim(data)[2]){
    temp <- as.data.frame(.liftGroup(data[,i],weight,bucket))

    if (newNameFlag) temp$ModelNames <- showas[i]
    else if (!is.null(names(data)[i])) temp$ModelNames <- names(data)[i]
    else temp$ModelNames <- paste("Model ",i,sep="")

    P <- rbind(P,temp)
  }

  rbokeh::figure(tools = .tools,width=800,height=500,ylab="Prediction") %>%
    rbokeh::ly_lines(Group, Pred, data = P, width=1.5, color = ModelNames) %>%
    rbokeh::ly_points(Group, Pred, data = P, size=10, color = ModelNames,
              hover="<strong>Model:</strong> @ModelNames<br><strong>y value:</strong> @Pred")
}

.liftGroup <- function(pred,weight=NULL,bucket){

  df <- data.frame(cbind(pred,weight))
  df <- df[order(df[,1]),]
  df$group <- ceiling((1:length(pred)) / ceiling(length(pred)/bucket))
  
  df_plot <-df %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(wmean = weighted.mean(pred, weight))
                                                                                                                                                                                                       
  names(df_plot) <- c("Group","Pred")
  df_plot
}

#global variable
globalVariables(c("Group","Pred","ModelNames","group"))
