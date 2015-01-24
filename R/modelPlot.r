#' modelPlot
#'
#' @description This function allows you visulise the GLM fitting by comparing mean fit, observation, and normal fit.
#' @usage modelPlot(model,xvar,type=c("response","link"),dataset=NULL,weights=NULL,by=NULL,modelType=c("glm","gbm"),interactive=FALSE,...)
#' @param model a model object. Currently this is created for glm and gbm. 
#' @param xvar a character string indicates the variable name you want to visulise.
#' @param type either "response" or "link". By default ("response") will plot on GLM response as oppose to linear predictor ("link").
#' @param data Optional. A data frame. update the glm model plot with a new dataset.
#' @param weights Optional. A numerical vector to specify the weights used for updating the glm model for plotting.
#' @param modelType A character string. Either "glm" or "gbm".
#' @param interactive logical. Set true to use googleVis package plotting interactively. Currently it doesn't work when using "by" method.
#' @param by Optinal. A character string indicates the variable name you want to plot the fit by.
#' @details 
#' For those used Emblem before, you will find this plot quite familiar.  The purpose of this function is the same that to put observation, fitted, and mean fit on the same plot for better understanding about model fitting.
#' 
#' As James said, you can also use loop, lm, gbm function to check the fit.  But I'm quite lazy that wants to put everthing together in the way I familiar with.
#' 
#' Please do not name the dataset as "data", which is confusing and may cause problems.
#' 
#' Observation line for xvar is simply the weighted average on target varaible, hence if the xvar is a numeric, the plot could be very messy. Will improve this in the future to banded those numeric factor.
#'   
#' Fitted line for xvar is the weighted average on model prediction.
#' 
#' Fitted_mean line for xvar is the predictions of the current model on a Mode dataset which keeps xvar as it is but fix all other variabel as its mean or mode.   
#' 
#' @author Sixiang Hu
#' @seealso gbm, glm
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
#' modelPlot(glm1,"cyl",modelType="glm",interactive=T)

modelPlot <- function(model,xvar,type=c("response","link"),dataset=NULL,weights=NULL,by=NULL,modelType=c("glm","gbm"),interactive=FALSE,...){
  require(data.table)
  require(ggplot2)
  require(gridExtra)
  require(scales)
  require(reshape2)
  require(splines)
  require(googleVis)
  
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
  
  #data source
  if(!is.null(dataset)) {
    cat("modelPlot: Refitting model using new data...\n")
    model<-update(model,data=dataset,weights=weights)
  }
  else{
    if ( ("data" %in% names(model)) && modelType=="glm" ) {
      #this method doesn't work for gbm, because gbm$data is not the raw data, but a list.
      dataset <- data.frame()
      dataset <- model$data
    }
    else {
      if (isS4(model)) call <- model@call
      else call <- model$call
      
      dataset <- eval(call$data)
    }
  }
  if(is.null(dataset)) stop("modelPlot: Cannot found any proper data set.  Variables not in a data frame persumably?")
  if(!xvar %in% colnames(dataset)) stop(paste("modelPlot: Selected variable (",xvar,") is not in the data.",""))
  
  #weights
  if (is.null(weights)) {
    if (modelType == "glm") weights <- model$prior.weights
    else if (modelType == "gbm") {
      w_name <- deparse(model$call$weights)
      if (w_name %in% names(dataset)) weights <- dataset[,deparse(model$call$weights)]
      else if ("weights" %in% names(model$call) ) weights <- model$call$weights
      else weights <- rep(1,length(model$fit))          
    }
  }
  
  #fitted.values
  fitted <- as.numeric(model$fit)
  
  #observed
  if (modelType == "glm") observed <- as.numeric(model$y)
  else if (modelType == "gbm") {
    if ( model$response.name %in% names(dataset) ) observed <- as.numeric(dataset[,model$response.name])
    else observed <- get(model$response.name)
  }

  #Calculate mean data set for mean_fit line
  MeanData <- ModeData(dataset,weights)
  MeanData[,xvar] <- dataset[,xvar]
  fitted_mean <- as.numeric(predict(model,MeanData,type=type,weights=weights))
  
  #Plotting
  options(warn=-1)
  if( !is.null(by) && !by %in% colnames(dataset) ) stop(paste("Selected factor (",by,") is not in the data.",""))
  else if ( !is.null(by) ) {
    data.plot <-data.table(as.data.frame(cbind(xvar=data[,xvar],by=data[,by],fitted,observed,weights),stringsAsFactors=FALSE))
    setkey(data.plot,xvar,by)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=list(xvar,by),.SDcols=c("fitted","observed","weights")]
    
    data.agg <- as.data.frame(data.plot[,lapply(.SD,weighted.mean,w=weights),by=list(xvar,by),.SDcols=c("fitted","observed","weights")],row.names=c("xvar","by","weights","fitted","observed"))
    
    data.freq <- as.data.frame(data.plot[,sum(weights),by=list(xvar,by)][,freq:=V1/sum(V1)])
    
    #any difference between trans before and after aggregate?
    if(type=="link") {
      data.agg$fitted <- tran_lnk_fun(data.agg$fitted)
      data.agg$observed <- tran_lnk_fun(data.agg$observed)
    }
    
    #line graph fitted
    gLine1 <- ggplot(data=data.agg,aes(x=xvar,y=fitted,group=by,colour=by))+geom_line(size=1.5)+geom_point(size=4,fill="white")
    if("xlim" %in% opts) gLine1 <-gLine1 + xlim(xlim)
    if("ylim" %in% opts) gLine1 <-gLine1 + ylim(ylim)
    gLine1 <- gLine1+xlab("")+ylab(type)+ ggtitle(paste("Fitting Analysis on: ",xvar," by ",by, "(Fitted)"))+
      theme_bw()+theme(legend.justification=c(1,1), legend.position=c(1,1),axis.text.x = element_blank())
    
    #line graph observed
    gLine2 <- ggplot(data=data.agg,aes(x=xvar,y=observed,group=by,colour=by))+geom_line(size=1.5)+geom_point(size=4,fill="white")
    if("xlim" %in% opts) gLine2 <-gLine2 + xlim(xlim)
    if("ylim" %in% opts) gLine2 <- gLine2 + ylim(ylim)
    gLine2 <- gLine2+xlab("")+ylab(type)+ ggtitle(paste("Fitting Analysis on: ",xvar," by ",by, "(Observed)"))+
      theme_bw()+theme(legend.justification=c(1,1), legend.position=c(1,1))
    if(nlevels(as.factor(data.agg$xvar))>25) gLine2 <- gLine2 + theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))
    
    #histogram graph
    ghist <- ggplot(data=data.freq,aes(x=xvar,y=freq,fill=by))+ geom_histogram(stat="identity",binwidth=1)
    if("xlim" %in% opts) ghist <-ghist + xlim(xlim)
    ghist <- ghist +xlab("")+ ylab("percent (%)")+ scale_y_continuous(labels = percent)+theme_bw()+
      theme(legend.justification=c(1,1), legend.position=c(1,1),axis.text.x = element_blank())
    
    grid.arrange(gLine1,gLine2,ghist,ncol=1,nrow=3,heights=c(4,4,2))
  }
  else {
    covf2c <- sapply(data, is.factor)
    data[covf2c] <- lapply(data[covf2c], as.character)
    
    #use data.table to speed up
    data.plot <-data.table(as.data.frame(cbind(xvar=data[,xvar],fitted,fitted_mean,observed,weights),stringsAsFactors=FALSE))
    setkey(data.plot,xvar)
    
    data.plot <- data.plot[,lapply(.SD,as.numeric),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")]
    data.agg <- as.data.frame(data.plot[,lapply(.SD,weighted.mean,w=weights),by=xvar,.SDcols=c("fitted","fitted_mean","observed","weights")],row.names=c("xvar","weights","fitted","fitted_mean","observed"))
    data.freq <- as.data.frame(data.plot[,sum(weights),by=xvar][,freq:=V1/sum(V1)])
    
    #any difference between trans before and after aggregate?
    if(type=="link") {
      tran_lnk_fun <- family(model)$linkfun
      data.agg$fitted <- tran_lnk_fun(data.agg$fitted)
      data.agg$observed <- tran_lnk_fun(data.agg$observed)
    }
    
    #melt
    data.melt <- melt(data.agg[,-5],id=c("xvar"))
    
    #line graph
    
    strV1 <- paste("Fitting Analysis on: ",xvar,"(",type,")")
    
    gLine <- ggplot(data=data.melt,aes(x=xvar,y=value,group=variable,colour=variable,shape=variable))+geom_line(size=1.5)+geom_point(size=4,fill="white")
    if(("xlim" %in% opts) && is.numeric(data.melt$xvar)) gLine <-gLine + scale_x_continuous(limits=xlim)
    else if(("xlim" %in% opts) && !is.numeric(data.melt$xvar)) gLine <-gLine + scale_x_discrete(limits=xlim)
    else if(("xlim" %in% opts) && is(data.melt[,"xvar"],"Date")) gLine <-gLine + scale_x_date(label=date_format("%y%m"),limits=xlim)
    if("ylim" %in% opts) gLine <-gLine + ylim(ylim)
    gLine <- gLine+scale_colour_manual(values=c("green4","green1","magenta3"))+scale_shape_manual(values=c(21,2,0))+xlab("")+ylab(type)+
      ggtitle(strV1)+
      theme_bw()+theme(legend.justification=c(1,1), legend.position=c(1,1))
    if(nlevels(as.factor(data.melt$xvar))>25) gLine <- gLine + theme(axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5))
    
    #histogram graph
    ghist <- ggplot(data=data.freq,aes(x=xvar,y=freq))+
      geom_histogram(stat="identity",colour="black",fill="yellow")
      if(("xlim" %in% opts) && is.numeric(data.melt$xvar)) ghist <-ghist + scale_x_continuous(limits=xlim)
      else if(("xlim" %in% opts) && !is.numeric(data.melt$xvar)) ghist <-ghist + scale_x_discrete(limits=xlim)
      #else if(("xlim" %in% opts) && is(data.freq[,"xvar"],"Date")) ghist <-ghist + scale_x_date(label=date_format("%y%m"),limits=xlim)
      ghist <- ghist + ylab("percent (%)")+
      scale_y_continuous(labels = percent)+
      xlab("")+theme_bw()+theme(axis.text.x = element_blank())
  
    grid.arrange(gLine,ghist,ncol=1,nrow=2,heights=c(4,1))
    
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
      
      plot(gvisComboChart(df,xvar="xvar",yvar=c("fitted","fitted_mean","observed","freq"),options=gvisSingleOptionList))
#       plot(gvisComboChart(df,xvar="xvar",yvar=c("fitted","fitted_mean","observed","freq"),
#                           options=list(series='{type:"line",color:"green4",targetAxisIndex:0},
#                                                     {type:"line",color:"green1",targetAxisIndex:0},
#                                                     {type:"line",color:"magenta3",targetAxisIndex:0},
#                                                     {type:"bars",color:"yellow",targetAxisIndex:1}',
#                                        crosshair="{trigger:'both'}",
#                                        height=800)))
      #   plot(gvisLineChart(df,xvar="xvar",yvar="fitted"))
    }
  }
  
  options(warn=0)
}