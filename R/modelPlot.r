#' modelPlot
#'
#' @description This function allows you visulise the GLM or GBM fitting by comparing observation, fitted and mean fitted on the same plot.
#' @usage modelPlot(model,xvar,type=c("response","link"),dataset=NULL,weights=NULL,by=NULL,modelType=c("glm","gbm"),interactive=FALSE,...)
#' @param model a model object. Currently this is created for glm and gbm. 
#' @param xvar a character string indicates the variable name you want to visulise.
#' @param type either "response" or "link". By default ("response") will plot on GLM response as oppose to linear predictor ("link").
#' @param data Optional. A data frame. update the glm model plot with a new dataset.
#' @param weights Optional. A numerical vector to specify the weights used for updating the glm model for plotting.
#' @param modelType A character string. One of "glm", "glm.nb" or "gbm".
#' @param interactive logical. Set true to use googleVis package plotting interactively. Currently it doesn't work when using "by" method.
#' @param by Optinal. A character string indicates the variable name you want to plot the fit by.
#' @param newGroupNum Optional. An integer specifies number of new bands when levels of current plotting variable `xvar` or `by` is more than 100. 
#' @param ... xlim and ylim can be used to set the range of the ggplot2 plot. For example, xlim=c(0,1) means restrict the xaxis within (0,1).  This does not work for goolgeVis interactive plot because, because, because it is interactive, which you can zoom in and out with your mouse. :)
#' @details 
#' For those used Emblem before, you will find this plot quite familiar.  The purpose of this function is the same that to put observation, fitted, and mean fit on the same plot for better understanding about model fitting.
#'  
#' Please do not name the dataset as "data", which is confusing and may cause problems.
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
#' @author Sixiang Hu
#' @seealso \code{\link{gbm}}, \code{\link{glm}}, \code{\link{plot.gbm}}
#' @export modelPlot
#' @examples
#' 
#' #model from gbm package
#' N <- 1000
#' X1 <- runif(N)
#' X2 <- 2*runif(N)
#' X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
#' X4 <- factor(sample(letters[1:6],N,replace=TRUE))
#' X5 <- factor(sample(letters[1:3],N,replace=TRUE))
#' X6 <- 3*runif(N) 
#' mu <- c(-1,0,1,2)[as.numeric(X3)]
#' 
#' SNR <- 10 
#' Y <- X1**1.5 + 2 * (X2**.5) + mu
#' sigma <- sqrt(var(Y)/SNR)
#' Y <- Y + rnorm(N,0,sigma)
#' 
#' X1[sample(1:N,size=500)] <- NA
#' X4[sample(1:N,size=300)] <- NA
#' 
#' data_raw <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
#' 
#' gbm1 <-
#' gbm(Y~X1+X2+X3+X4+X5+X6,         
#'     data=data_raw,                   
#'     var.monotone=c(0,0,0,0,0,0), 
#'     distribution="gaussian",     
#'     n.trees=1000,                
#'     shrinkage=0.05,              
#'     interaction.depth=3,         
#'     bag.fraction = 0.5,          
#'     train.fraction = 0.5,        
#'     n.minobsinnode = 10,         
#'     cv.folds = 3,                
#'     keep.data=TRUE,              
#'     verbose=FALSE,               
#'     n.cores=1)
#' 
#' modelPlot(gbm1,xvar="X3",modelType="gbm",by="X5")
#' 
#' #a glm example
#' 
#' glm1 <- glm(formula = mpg ~ cyl + hp, family = Gamma(log), data = mtcars, weights = wt)
#' 
#' modelPlot(glm1,"cyl",modelType="glm",interactive=TRUE)

modelPlot <- function(model,
                      xvar,
                      type=c("response","link"),
                      dataset=NULL,
                      weights=NULL,
                      by=NULL,
                      modelType=c("glm","glm.nb","gbm"),
                      interactive=FALSE,
                      newGroupNum=10,
                      ...){

  type <- match.arg(type)
  opts.list<-list(...)
  opts <- names(list(...))
  if("xlim" %in% opts) xlim<-opts.list$xlim
  if("ylim" %in% opts) ylim<-opts.list$ylim
  if("binwidth" %in% opts) binwidth<-opts.list$binwidth else binwidth <- 1
  
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
  
  #observed
  if (modelType %in% c("glm","glm.nb")) observed <- as.numeric(model$y)
  else if (modelType == "gbm") {
    if ( model$response.name %in% names(dataset) ) observed <- as.numeric(dataset[,model$response.name])
    else if ( model$gbm.call$response.name %in% names(dataset) )  observed <- as.numeric(dataset[,model$gbm.call$response.name])
    else observed <- get(model$response.name)
  }

  #Calculate mean data set for mean_fit line
  MeanData <- ModeData(dataset,weights)
  MeanData[,xvar] <- dataset[,xvar]
  
  #New Group for data which has too much levels.
  if ( (is.numeric(dataset[,xvar]) || is.integer(dataset[,xvar])) && nlevels(as.factor(dataset[,xvar]))>100 ) {
    if ( is.null(newGroupNum) ) newGroupNum <- 10
    
    new_band <- seq(min(dataset[,xvar]),max(dataset[,xvar]),length.out=newGroupNum)
    dataset[,xvar] <- cut(dataset[,xvar],new_band,include.lowest = TRUE)
  }
  
  #fitted mean
  if (modelType == "glm") fitted_mean <- as.numeric(predict(model,MeanData,type=type,weights=weights))
  else if (modelType == "glm.nb") fitted <- as.numeric(predict(model,MeanData,type=type))
  else if (modelType == "gbm") fitted_mean <- as.numeric(predict(model,MeanData,type=type,weights=weights,n.trees=model$n.trees))
  
  #Plotting
  options(warn=-1)
  
  covf2c <- sapply(dataset, is.factor)
  dataset[covf2c] <- lapply(dataset[covf2c], as.character)
  
  #any difference between trans before and after aggregate?
  if(type=="link") {
    tran_lnk_fun <- family(model)$linkfun
    fitted <- tran_lnk_fun(fitted)
    observed <- tran_lnk_fun(observed)
  }
  
  if( !is.null(by) && !by %in% colnames(dataset) ) stop(paste("Selected factor (",by,") is not in the data.",""))
  else if ( !is.null(by) ) {
    
    if ( (is.numeric(dataset[,by]) || is.integer(dataset[,by])) && nlevels(as.factor(dataset[,by]))>100 ) {
      if ( is.null(newGroupNum) ) newGroupNum <- 10
      
      new_band <- seq(min(dataset[,by]),max(dataset[,by]),length.out=newGroupNum)
      dataset[,by] <- cut(dataset[,by],new_band,include.lowest = TRUE)
    }
    
    data.plot <-data.table(as.data.frame(cbind(xvar=dataset[,xvar],by=dataset[,by],fitted,observed,weights),stringsAsFactors=FALSE))
    setkey(data.plot,xvar,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(xvar,by),.SDcols=c("fitted","observed","weights")]
    
    data.agg <- as.data.frame(data.plot[,lapply(.SD,weighted.mean,w=weights),by=list(xvar,by),.SDcols=c("fitted","observed","weights")],row.names=c("xvar","by","weights","fitted","observed"))
    
    data.freq <- as.data.frame(data.plot[,sum(weights),by=list(xvar,by)][,freq:=V1/sum(V1)])
        
    #line graph fitted
    gLine1 <- ggplot2::ggplot(data=data.agg,aes(x=xvar,y=fitted,group=by,colour=by))+
      ggplot2::geom_line(size=1) + ggplot2::geom_point(size=4,fill="white")
    if("xlim" %in% opts) gLine1 <-gLine1 + xlim(xlim)
    if("ylim" %in% opts) gLine1 <-gLine1 + ylim(ylim)
    gLine1 <- gLine1+ggplot2::xlab("")+ggplot2::ylab(type)+ ggplot2::ggtitle(paste("Fitting Analysis on: ",xvar," by ",by, "(Fitted)"))+
      theme_mp_line
    
    #line graph observed
    gLine2 <- ggplot2::ggplot(data=data.agg,aes(x=xvar,y=observed,group=by,colour=by))+
      ggplot2::geom_line(size=1) + ggplot2::geom_point(size=4,fill="white")
    if("xlim" %in% opts) gLine2 <-gLine2 + ggplot2::xlim(xlim)
    if("ylim" %in% opts) gLine2 <- gLine2 + ggplot2::ylim(ylim)
    gLine2 <- gLine2+ggplot2::xlab("")+ggplot2::ylab(type)+ ggplot2::ggtitle(paste("Fitting Analysis on: ",xvar," by ",by, "(Observed)"))+
      theme_mp_line
    if(nlevels(as.factor(data.agg$xvar))>25) gLine2 <- gLine2 + ggplot2::theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))
    
    #histogram graph
    ghist <- ggplot2::ggplot(data=data.freq,aes(x=xvar,y=freq,fill=by))+ ggplot2::geom_histogram(stat="identity",binwidth=1)
    if("xlim" %in% opts) ghist <-ghist + ggplot2::xlim(xlim)
    ghist <- ghist + ggplot2::xlab("")+ ggplot2::ylab("percent (%)")+ ggplot2::scale_y_continuous(labels = percent) + 
      theme_mp_hist
    
    gridExtra::grid.arrange(gLine1,gLine2,ghist,ncol=1,nrow=3,heights=c(4,4,2))
  }
  else {

    #use data.table to speed up
    data.plot <-data.table::data.table(as.data.frame(cbind(xvar=dataset[,xvar],fitted,fitted_mean,observed,weights),stringsAsFactors=FALSE))
    data.table::setkey(data.plot,xvar)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")]
    data.agg <- as.data.frame(data.plot[,lapply(.SD,weighted.mean,w=weights),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")],row.names=c("xvar","weights","fitted","fitted_mean","observed"))
    data.freq <- as.data.frame(data.plot[,sum(weights),by=xvar][,freq:=V1/sum(V1)])

    #melt
    data.melt <- reshape2::melt(data.agg[,-5],id=c("xvar"))
    
    #line graph
    
    strV1 <- paste("Fitting Analysis on: ",xvar,"(",type,")")
    
    gLine <- ggplot2::ggplot(data=data.melt,aes(x=xvar,y=value,group=variable,colour=variable,shape=variable)) + 
      ggplot2::geom_line(size=1) + ggplot2::geom_point(size=4,fill="white")
    if(("xlim" %in% opts) && is.numeric(data.melt$xvar)) gLine <-gLine + ggplot2::scale_x_continuous(limits=xlim)
    else if(("xlim" %in% opts) && !is.numeric(data.melt$xvar)) gLine <-gLine + ggplot2::scale_x_discrete(limits=xlim)
    else if(("xlim" %in% opts) && is(data.melt[,"xvar"],"Date")) gLine <-gLine + ggplot2::scale_x_date(label=date_format("%y%m"),limits=xlim)
    if("ylim" %in% opts) gLine <-gLine + ggplot2::ylim(ylim)
    gLine <- gLine + ggplot2::scale_colour_manual(values=c("green4","green1","magenta3")) + ggplot2::scale_shape_manual(values=c(21,24,22)) + ggplot2::xlab("") + ggplot2::ylab(type)+
      ggplot2::ggtitle(strV1)+ theme_mp_line
    if(nlevels(as.factor(data.melt$xvar))>25) gLine <- gLine + ggplot2::theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))
    
    #histogram graph
    ghist <- ggplot2::ggplot(data=data.freq,aes(x=xvar,y=freq))+
      ggplot2::geom_histogram(stat="identity",colour="black",fill="yellow")
      if(("xlim" %in% opts) && is.numeric(data.melt$xvar)) ghist <-ghist + ggplot2::scale_x_continuous(limits=xlim)
      else if(("xlim" %in% opts) && !is.numeric(data.melt$xvar)) ghist <-ghist + ggplot2::scale_x_discrete(limits=xlim)
      ghist <- ghist + ggplot2::ylab("percent (%)")+
      ggplot2::scale_y_continuous(labels = percent)+
      ggplot2::xlab("")+ theme_mp_hist
  
    gridExtra::grid.arrange(gLine,ghist,ncol=1,nrow=2,heights=c(4,1))
    
    if (interactive) {
      df <- data.frame(data.agg,freq=data.freq$freq)
      gvisSingleOptionList <- list(pointSize=8,
                                   series="[{targetAxisIndex:0, type:'line',color:'greenyellow',pointShape: 'circle'},
{targetAxisIndex:0, type:'line',color:'green',pointShape: 'triangle'},
{targetAxisIndex:0, type:'line',color:'magenta',pointShape: 'square'},
{targetAxisIndex:1, type:'bars',color:'yellow'}]",
                                   crosshair="{trigger:'both'}",
                                   hAxis.title=strV1,
                                   theme="maximized",
                                   title=paste0("Fitting analysis on ",strV1, " Observed"),
                                   vAxes="{1:{format:'##.#%',maxValue:1}}",
                                   explorer="{ actions: ['dragToZoom', 'rightClickToReset'],keepInBounds: true }",
                                   chartArea="{width:'90%',height:'90%'}",
                                   height=750)
      
      plot(googleVis::gvisComboChart(df,xvar="xvar",yvar=c("fitted","fitted_mean","observed","freq"),options=gvisSingleOptionList))

    }
  }
  
  options(warn=0)
}

# Create mode or average of a dataset
ModeData <- function(data,weights){

  if(!(class(data) %in% c("data.frame","ore.frame"))){
    if(length(data)==0) stop("data set is empty.")
  }
  else{
    if(dim(data)[1]>0 && dim(data)[2]>0 ){
      VarName <- names(data)
      iLen <- length(VarName)
    }
    else stop("data set is empty.")
  }
  
  if(is.null(weights)) weights<-rep(1,length(data[,1]))
  
  for(i in 1:iLen) {
    x_dt<-data.table::data.table(x=data[,VarName[i]],weights)
    data[,VarName[i]] <- rep(data.frame(x_dt[,sum(weights),by=x][order(-V1)])[1,1],length(data[,1]))
  }
  
  return(data)
}

#theme for plot ggplot2 graph
theme_mp_line <- ggplot2::theme_bw() + 
  ggplot2::theme(text=element_text(face="bold.italic"),
                 legend.justification=c(1,1),
                 legend.position=c(1,1))


theme_mp_hist <- ggplot2::theme_bw() + 
  ggplot2::theme(text=element_text(face="bold.italic"),
                 axis.text.x = element_blank())
