#rbokeh tool sets
.tools <- c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save")

#global variable
globalVariables(c("V1",".N","f","N"))

#return position index and name of a given variable in the data
.VarPosition <- function(data,var){
  tmp <- list(posi=NULL,name=NULL)
  
  if (is.character(var)) {
    if(!var %in% colnames(data)) 
      stop(paste("Variable (",var,") cannot be found in the data provided.",""))
    
    tmp$posi <- match(var,names(data))
    tmp$name <- var
  }
  else if (is.integer(var)) { 
    if (var > dim(data)[2])
      stop("Position specified: (",var,") is outside of the data.")
    tmp$posi <- var
    tmp$name <- names(data)[var]
  }
  
  if (is.null(tmp$posi))
    stop ("xvar provided is either a character (variable name) or integer (position of the variable).")
  else tmp
}

#whether a dataset is data frame.
.isDFnull <- function(data){
  ("data.frame" %in% class(data)) && (dim(data)[1]==0 || dim(data)[2]==0)
}

#Mean Data For Fitted Mean
.ModeData <- function(data,weights,base){
  
  if(!("data.frame" %in% class(data))){
    if(length(data)==0) stop("data set is empty.")
  }
  else{
    if( !.isDFnull(data) ){
      VarName <- names(data)
      iLen <- length(VarName)
    }
    else stop("data set is empty.")
  }
  
  if(is.null(weights)) weights<-rep(1,dim(data)[1])
  
  for(i in 1:iLen) {
    x_dt<-data.table::as.data.table(cbind(x=data[,VarName[i]],weights))
    
    if( sum(c("character","factor") %in% class(data[,VarName[i]]))>0 )
      if (!is.null(base[[VarName[i]]])) data[,VarName[i]] <- as.character(base[[VarName[i]]])
      else data[,VarName[i]] <- as.character(x_dt[,sum(weights),by=x][order(-V1)][1,1,with=FALSE])
    else
      if (!is.null(base[[VarName[i]]])) data[,VarName[i]] <- base[[VarName[i]]]
      else data[,VarName[i]] <- x_dt[,sum(weights),by=x][order(-V1)][1,1,with=FALSE]
  }
  
  return(data)
}
