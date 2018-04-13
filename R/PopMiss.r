#' Populate Missing Value
#'
#' @description This function allows you to populate missing values. 
#' @usage PopMiss(data,na.treatment=c("mean.or.mode","delete","replace"),replace=NULL)
#' @param data This could be data frame,data.table, matrix or a vector.
#' @param na.treatment One of "mean.or.mode", "delete", or "replace", to specify the method to populate missing values.
#' @param replace A single value used for populating ALL missing value.  May not be useful for a data frame with missing values in different type of variable. 
#' @seealso DetMiss
#' @author Sixiang Hu
#' @importFrom stats na.omit
#' @export
#' @examples
#' a <- c(sample(LETTERS,5),NA)
#' PopMiss(a,"mean.or.mode")

PopMiss <- function(data,na.treatment=c("mean.or.mode","delete","replace"),replace=NULL){
  if (is.null(na.treatment)) na.treatment <- "mean.or.mode"
  
  na.treatment <- match.arg(na.treatment)
  
  if ((na.treatment %in% c("replace","r")) && is.null(replace)) 
    stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
  
  UseMethod("PopMiss",data)
}

#' @export
#' @rdname PopMiss
PopMiss.factor<-function(data,na.treatment,replace){
  PopMiss.character(data,na.treatment,replace)
}

#' @export
#' @rdname PopMiss
PopMiss.character<-function(data,na.treatment,replace){
  num_miss <- sum(is.na(data))
  if(identical(na.treatment,"delete")) {
    return(data[!is.na(data)])
  }
  else if(identical(na.treatment,"mean.or.mode")){
    replace <- names(table(data))[order(table(data),decreasing=TRUE)[1]]
  }
  
  data[is.na(data)] <- replace
  return(data)
}

#' @export
#' @rdname PopMiss
PopMiss.integer<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  if(identical(na.treatment,"delete")) {
    return(na.omit(data))
  }
  else if(identical(na.treatment,"mean.or.mode")){
    replace <- floor(mean(data,na.rm=TRUE))
  }
  data[is.na(data)] <- replace
  return(data)
}

#' @export
#' @rdname PopMiss
PopMiss.Date<-function(data,na.treatment,replace){
  PopMiss.character(data,na.treatment,replace)
}

#' @export
#' @rdname PopMiss
PopMiss.numeric<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  if(identical(na.treatment,"delete")) {
        return(na.omit(data))
  }
  else if(identical(na.treatment,"mean.or.mode")){
    replace <- mean(data,na.rm=TRUE)
  }
  data[is.na(data)] <- replace
  return(data)
}

#' @export
#' @rdname PopMiss
PopMiss.data.frame<-function(data,na.treatment,replace){
  num_miss <- which(sapply(data,function(x) any(is.na(x))))
  if(identical(na.treatment,"delete")) {
    return(na.omit(data))
  }
  else if(identical(na.treatment,"mean.or.mode")){
    ind <- dim(data)[2]
    for(i in 1:ind){
      if(class(data[[i]]) %in% c("character","Date","factor"))
        replace <- names(table(data[[i]]))[order(table(data[[i]]),decreasing=TRUE)[1]]
      else if(class(data[[i]])=="integer" )
        replace <- floor(mean(data[[i]],na.rm=TRUE))
      else 
        replace <- mean(data[[i]],na.rm=TRUE)
      data[[i]][is.na(data[[i]])] <- replace
    }
    
    return(data)
  }
  else{
    warning("Provided replacement will be used for all missing value.")
    ind <- dim(data)[2]
    for(i in 1:ind){
    	if ("Date" %in% class(data[[i]]) ) 
    	  data[[i]][is.na(data[[i]])] <- as.Date("1960-01-01")	
      else 
        data[[i]][is.na(data[[i]])] <- replace
    }
    return(data)
  }
}

#' @export
#' @rdname PopMiss
PopMiss.data.table <-function(data,na.treatment,replace){
  PopMiss.data.frame(data,na.treatment,replace)
}

#' @export
#' @rdname PopMiss
PopMiss.matrix <- function(data,na.treatment,replace){
	if(identical(na.treatment,"delete")) 
	  warning("na.treatment is \"delete\" and 'data' is a matrix. This only works correctly if whole rows are missing")
	matrix(PopMiss(as.vector(data),na.treatment,replace),ncol=ncol(data))
}

#global variable
globalVariables(c("V1",".N","f","N",".SD"))