#' Populate Missing Value
#'
#' @description This function allows you to populate missing values. 
#' @usage PopMiss(data,na.treatment=c("mean.or.mode","delete","replace"),replace=NULL)
#' @param data This could be data frame, matrix or a vector.
#' @param na.treatment One of "mean.or.mode", "delete", or "replace", to specify the method to populate missing values.
#' @param replace A single value used for populating ALL missing value.  May not be useful for a data frame with missing values in different type of variable. 
#' @seealso DetMiss
#' @author Sixiang Hu
#' @export
#' @examples
#' a <- c(sample(LETTERS,5),NA)
#' PopMiss(a,"mean.or.mode")

PopMiss <- function(data,na.treatment=c("mean.or.mode","delete","replace"),replace=NULL){
  na.treatment <- match.arg(na.treatment)
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
  if(isTRUE(na.treatment=="delete")) {
    return(data[!is.na(data)])
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    replace <- names(table(data))[order(table(data),decreasing=TRUE)[1]]
  }
  
  if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
  data[is.na(data)] <- replace
  return(data)
}

#' @export
#' @rdname PopMiss
PopMiss.integer<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  if(isTRUE(na.treatment=="delete")) {
    return(na.omit(data))
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    replace <- floor(mean(data,na.rm=TRUE))
  }
  if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
  data[is.na(data)] <- replace
  return(data)
}

#' @export
#' @rdname PopMiss
PopMiss.numeric<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  if(isTRUE(na.treatment=="delete")) {
    cat(paste(num_miss," rows have been delete.\n\n"))
        return(na.omit(data))
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    replace <- mean(data,na.rm=TRUE)
  }
  if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
  
  data[is.na(data)] <- replace
  cat(paste(num_miss," row have been populated by value:",replace,".\n\n"))
  return(data)
}

#' @export
#' @rdname PopMiss
PopMiss.data.frame<-function(data,na.treatment,replace){
  num_miss <- which(sapply(data,function(x) any(is.na(x))))
  if(isTRUE(na.treatment=="delete")) {
    return(na.omit(data))
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    str_name <- names(data)
    for(i in num_miss){
      if(class(data[,str_name[i]]) %in% c("character","date","factor"))
        replace <- names(table(data[,str_name[i]]))[order(table(data[,str_name[i]]),decreasing=TRUE)[1]]
      else if(class(data[,str_name[i]])=="integer" )
        replace <- floor(mean(data[,str_name[i]],na.rm=TRUE))
      else 
        replace <- mean(data[,str_name[i]],na.rm=TRUE)
      data[is.na(data[,str_name[i]]),str_name[i]] <- replace
    }
    
    return(data)
  }
  else{
    if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
    
    warning("Provided replacement will be used for all missing value.")
    str_name <- names(data)
    for(i in num_miss){
    	if ("Date" %in% class(data[,str_name[i]]) ) data[is.na(data[,str_name[i]]),str_name[i]] <- as.Date("1960-01-01")	
      	else data[is.na(data[,str_name[i]]),str_name[i]] <- replace
    }
    return(data)
  }
}

#' @export
#' @rdname PopMiss
PopMiss.data.table <-function(data,na.treatment,replace){
  num_miss <- which(data[,sapply(.SD,function(x) any(is.na(x)))])
  if(isTRUE(na.treatment=="delete")) {
    return(na.omit(data))
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    str_name <- names(data)
    for(i in num_miss){
      if( data[,class(get(str_name[i]))] %in% c("character","date","factor"))
        replace <- as.data.frame(data[,.N,get(str_name[i])][order(-N)])[1,1]
      else if( data[,class(get(str_name[i]))]=="integer" )
        replace <- data[,floor(mean(get(str_name[i]),na.rm=TRUE))]
      else 
        replace <- data[,mean(get(str_name[i]),na.rm=TRUE)]
      
      data[which(is.na(data[,i,with=FALSE])),i] <- replace
    }
    
    return(data)
  }
  else{
    if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
    
    warning("Provided replacement will be used for all missing value.")
    str_name <- names(data)
    for(i in num_miss){
     	if ("Date" %in% class(data[,i,with=FALSE])) data[which(is.na(data[,i,with=FALSE])),i] <- as.Date("1960-01-01")	
      	data[which(is.na(data[,i,with=FALSE])),i] <- replace
    }
    return(data)
  }
}

#' @export
#' @rdname PopMiss
PopMiss.matrix <- function(data,na.treatment,replace){
	if(identical(na.treatment,"delete")) warning("na.treatment is \"delete\" and 'data' is a matrix. This only works correctly if whole rows are missing")
	matrix(PopMiss(as.vector(data),na.treatment,replace),ncol=ncol(data))
}

#global variable
globalVariables(c("V1",".N","f","N",".SD"))