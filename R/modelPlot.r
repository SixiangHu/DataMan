#' modelPlot
#'
#' @description This function allows you visulise the GLM or GBM fitting by comparing observation, fitted and mean fitted on the same plot.
#' @usage modelPlot(model,xvar,type=c("response","link"),dataset=NULL,weights=NULL,
#' by=NULL,modelType=c("glm","glm.nb","gbm","train"),newGroupNum=10)
#' @param model a model object. Currently this function supports glm, gbm and train object create by caret package. 
#' @param xvar a character string indicates the variable name you want to visulise.
#' @param type either "response" or "link". By default ("response") will plot on GLM response as oppose to linear predictor ("link").
#' @param dataset Optional. A data frame. update the glm model plot with a new dataset.
#' @param weights Optional. A numerical vector to specify the weights used for updating the glm model for plotting.
#' @param modelType A character string. One of "glm", "glm.nb", "train" or "gbm".
#' @param by Optinal. A character string indicates the variable name you want to plot the fit by.
#' @param newGroupNum Optional. An integer specifies number of new bands when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @details 
#' For those used Emblem before, you will find this plot quite familiar.  The purpose of this function is the same that to put observation, fitted, and mean fit on the same plot for better understanding about model fitting.
#' 
#' Observation line for xvar is simply the weighted average on target varaible, hence if xvar is a numeric, the plot could be very messy. Will improve this in the future to banded those numeric factor.
#'   
#' Fitted line for xvar is the weighted average on model prediction.
#' 
#' Fitted_mean line for xvar is the predictions of the current model on a Mode dataset which keeps xvar as it is but fix all other variabel as its mean or mode.   
#' 
#' For GBM model, currently, can only create response plot.  Because of the fact that link function is not explicitly saved in its model object hence I need to create link function for each of the GBM family.  Will think about how to do this.
#' 
#' The difference between `plot.gbm` in `gbm` package and this modelPlot function is that `plot.gbm` is plotting "marginal effect" as oppose to overall fitting.    
#' 
#' It is now support the `caret` package's `train` model.
#' 
#' @author Sixiang Hu
#' @importFrom data.table as.data.table data.table setkey :=
#' @importFrom rbokeh figure ly_lines ly_points ly_hist grid_plot
#' @seealso \code{\link{glm}}
#' @export modelPlot
#' @examples
#' 
#' ## glm example
#' 
#' glm1 <- glm(formula = mpg ~ cyl + hp, family = Gamma(log), data = mtcars, weights = wt)
#' modelPlot(glm1,"cyl",modelType="glm")

modelPlot <- function(model,
                      xvar,
                      type=c("response","link"),
                      dataset=NULL,
                      weights=NULL,
                      by=NULL,
                      modelType=c("glm","glm.nb","gbm","train"),
                      newGroupNum=10){
  
  type <- match.arg(type)

  modelType <- match.arg(modelType)
  if (modelType == "gbm" && type =="link") warning("modelPlot: no link function in gbm, using response for plotting.\n")
  
  if(!exists(deparse(substitute(model)))){
    stop("Model provided is blank.")
  }
  
  #weights
  if (is.null(weights)) {
    if (modelType == "glm") weights <- model$prior.weights
    else if ("weights" %in% names(model) ) weights <- model$weights
    else if (modelType == "gbm") {
      w_name <- deparse(model$call$weights)
      if (w_name %in% names(dataset)) weights <- dataset[,deparse(model$call$weights)]
      else if ("weights" %in% names(model$call) ) weights <- model$call$weights
      else weights <- rep(1,length(model$fit))          
    }
    else if (modelType == "train"){
      if ("weights" %in% names(model$call) ) weights <- model$call$weights
      else weights <- rep(1,dim(model$trainingData)[1])     
    }
  }
  
  #data source
  if(!is.null(dataset)) {
    if (modelType=="glm"){
      cat("modelPlot: Refitting model using new data...\n")
      model<-update(model,data=dataset,weights=weights)
    }
    else if (modelType=="glm.nb"){
      model<-update(model,data=dataset)
    }
  }
  else{
    if ( ("data" %in% names(model)) && modelType=="glm" ) {
      #this method doesn't work for gbm, because gbm$data is not the raw data, but a list.
      dataset <- model$data
    }
    else if ( modelType=="train" ){
      if (is.null(model$trainingData) ) stop("Model data in the `train` object is blank, please provide the raw data.")
      else dataset <- model$trainingData
    }
    else if ( !is.null(model$gbm.call$dataframe) ) dataset <- model$gbm.call$dataframe
    else {
      if (isS4(model)) call <- model@call
      else call <- model$call
      
      dataset <- eval(call$data)
    }
  }
  
  if(is.null(dataset)) stop("modelPlot: Cannot found any proper data set.  Variables not in a data frame persumably?")
  if(!xvar %in% colnames(dataset)) stop(paste("modelPlot: Selected variable (",xvar,") is not in the data.",""))
  
  #fitted.values
  if (modelType == "glm") fitted <- as.numeric(predict(model,dataset,type=type,weights=weights))
  else if (modelType == "glm.nb") fitted <- as.numeric(predict(model,dataset,type=type))
  else if (modelType == "gbm") fitted <- as.numeric(predict(model,dataset,type=type,weights=weights,n.trees=model$n.trees))
  else if (modelType == "train" ) fitted <- as.numeric(predict(model,dataset))
  
  #observed
  if (modelType %in% c("glm","glm.nb")) observed <- as.numeric(model$y)
  else if (modelType == "gbm") {
    if ( model$response.name %in% names(dataset) ) observed <- as.numeric(dataset[,model$response.name])
    else if ( model$gbm.call$response.name %in% names(dataset) )  observed <- as.numeric(dataset[,model$gbm.call$response.name])
    else observed <- get(model$response.name)
  }
  else if (modelType == "train") {
    observed <- dataset[,".outcome"]
  }
  
  #Calculate mean data set for mean_fit line
  MeanData <- .ModeData(dataset,weights)
  MeanData[,xvar] <- dataset[,xvar]

  #New Group for data which has too much levels.
  if ( (is.numeric(dataset[,xvar]) || is.integer(dataset[,xvar])) && nlevels(as.factor(dataset[,xvar]))>100 ) {
    if ( is.null(newGroupNum) ) newGroupNum <- 10
    
    new_band <- seq(min(dataset[,xvar], na.rm = TRUE),max(dataset[,xvar], na.rm = TRUE),length.out=newGroupNum)
    dataset[,xvar] <- as.character(cut(dataset[,xvar],new_band,include.lowest = TRUE))
  }
  
  #fitted mean
  if (modelType == "glm") fitted_mean <- as.numeric(predict(model,MeanData,type=type,weights=weights))
  else if (modelType == "glm.nb") fitted_mean <- as.numeric(predict(model,MeanData,type=type))
  else if (modelType == "gbm") fitted_mean <- as.numeric(predict(model,MeanData,type=type,weights=weights,n.trees=model$n.trees))
  else if (modelType == "train" ) fitted_mean <- as.numeric(predict(model,MeanData))
  
  #Plotting
  covf2c <- sapply(dataset, is.factor)
  dataset[covf2c] <- lapply(dataset[covf2c], as.character)
  
  strTitle <- paste("Fitting Analysis on: ",xvar,"(",type,")")
 
  #any difference between trans before and after aggregate?
  if(type=="link") {
    tran_lnk_fun <- family(model)$linkfun
    fitted <- tran_lnk_fun(fitted)
    observed <- tran_lnk_fun(observed)
  }
  
  if( !is.null(by) && (!by %in% colnames(dataset)) ) stop(paste("Selected factor (",by,") is not in the data.",""))
  else if ( !is.null(by) ) {
    
    strTitle <- paste(strTitle," by ",by,sep="")
    by_val <- dataset[,by]
    
    if ( (is.numeric(by_val) || is.integer(by_val)) && nlevels(as.factor(by_val))>100 ) {
      if ( is.null(newGroupNum) ) newGroupNum <- 10
      
      new_band <- seq(min(by_val, na.rm = TRUE),max(by_val, na.rm = TRUE),length.out=newGroupNum)
      by_val <- cut(by_val,new_band,include.lowest = TRUE)
    }
    by_val <- as.character(by_val)
    
    data.plot <- data.table::as.data.table(as.data.frame(cbind(xvar=dataset[,xvar],by=by_val,fitted,observed,weights),stringsAsFactors=FALSE))
    setkey(data.plot,xvar,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(xvar,by),.SDcols=c("fitted","observed","weights")]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=list(xvar,by),.SDcols=c("fitted","observed","weights")]

    #line graph fitted
    p1 <- rbokeh::figure(tools = .tools,title=strTitle,xlab="",ylab=type,height=500) %>%
      rbokeh::ly_lines(xvar,fitted,color=by,type=list(2),
                       width=2,data=data.agg) %>%
      rbokeh::ly_points(xvar,fitted,color=by,
                        data=data.agg,glyph=2,size=10,
                        hover="<strong>x value:</strong> @xvar<br><strong>fitted value:</strong> @fitted<br><strong>by value:</strong> @by") %>%
      rbokeh::ly_lines(xvar,observed,color=by,
                       width=2,data=data.agg) %>% 
      rbokeh::ly_points(xvar,observed,color=by,
                        data=data.agg,glyph=0,size=10,
                        hover="<strong>x value:</strong> @xvar<br><strong>obsered value:</strong> @observed<br><strong>by value:</strong> @by")

    #histogram graph
    if (sum(c("integer","numeric") %in% class(data.plot[,xvar]))>0)
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height=250) %>%
      rbokeh::ly_hist(xvar,data=data.plot)
    else
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height=250) %>%
      rbokeh::ly_bar(xvar,color=by,data=data.plot)
      
    grid_plot(list(p1,p2),ncol=1,nrow=2,width=900,same_axes = c(TRUE,FALSE))
  }
  else {
    #use as.data.table to speed up
    data.plot <-data.table::as.data.table(as.data.frame(cbind(xvar=dataset[,xvar],fitted,fitted_mean,observed,weights),stringsAsFactors=FALSE))
    setkey(data.plot,xvar)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")]
    data.agg  <- data.plot[,lapply(.SD,weighted.mean,w=weights),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")]
    
    #line graph
    p1 <- rbokeh::figure(xlab="",ylab=type,title=strTitle,
                         tools=.tools,height=500) %>%
      rbokeh::ly_lines(xvar,fitted,color="#336633",data=data.agg,
                       width=1,type=2,legend="Fitted") %>%
      rbokeh::ly_points(xvar,fitted,color="#336633",size=10,
                        data=data.agg,glyph=24,
                        hover="<strong>x value:</strong> @xvar<br><strong>fitted value:</strong> @fitted") %>%
      rbokeh::ly_lines(xvar,observed,color="#CC3399",data=data.agg,
                       width=1,legend="Observed")%>% 
      rbokeh::ly_points(xvar,observed,color="#CC3399",size=10,
                        data=data.agg,glyph=0,
                        hover="<strong>x value:</strong> @xvar<br><strong>obs value:</strong> @observed") %>%
      rbokeh::ly_lines(xvar,fitted_mean,color="#33CC33",data=data.agg,
                     width=1,legend="fitted_mean")%>% 
      rbokeh::ly_points(xvar,fitted_mean,color="#33CC33",size=10,
                        data=data.agg,glyph=25,
                        hover="<strong>x value:</strong> @xvar<br><strong>fitted mean value:</strong> @fitted_mean")
    
    #histogram graph
    if (sum(c("integer","numeric") %in% class(data.plot[,xvar]))>0)
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height=250) %>%
      rbokeh::ly_hist(xvar,data=data.plot)
    else
      p2 <- rbokeh::figure(xlab="",ylab="Frequency",height=250) %>%
      rbokeh::ly_bar(xvar,data=data.plot)
    
    grid_plot(list(p1,p2),ncol=1,nrow=2,width=900,same_axes = c(TRUE,FALSE))
  }
}