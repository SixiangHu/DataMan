#' Populate Missing Value
#'
#' @description This function allows you to populate missing values. 
#' @usage PopMiss(data)
#' @param data This could be data frame, matrix or a vector.
#' @param na.treatment One of "mean.or.mode", "delete", or "replace", to specify the method to populate missing values.
#' @param replace A single value used for populating ALL missing value.  May not be useful for a data frame with missing values in different type of variable. 
#' @seealso DetMiss
#' @author Sixiang Hu
#' @export
#' @examples
#' a <- c(sample(LETTERS,5),NA)
#' PopMiss(a)

setGeneric("PopMiss",function(data,
                              na.treatment=c("mean.or.mode","delete","replace"),
                              replace=NULL)
           standardGeneric("PopMiss")
  )

anyNA <- function(x){
  if(getRversion() < "3.1.0") any(is.na(x))
  else anyNA(x)
}

PopMiss.factor<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  na.treatment <- match.arg(na.treatment)
  if(isTRUE(na.treatment=="delete")) {
    cat(paste(num_miss," rows have been deleted.\n\n"))
    return(na.omit(data))
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    replace <- names(table(data))[order(table(data),decreasing=TRUE)[1]]
  }
  
  if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
  data[is.na(data)] <- replace
  cat(paste(num_miss," row have been populated by value: \"",replace,"\".\n\n"))
  return(data)
}

PopMiss.integer<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  na.treatment <- match.arg(na.treatment)
  if(isTRUE(na.treatment=="delete")) {
    cat(paste(num_miss," rows have been delete.\n\n"))
        return(na.omit(data))
  }
  else if(isTRUE(na.treatment=="mean.or.mode")){
    replace <- floor(mean(data,na.rm=TRUE))
  }
  if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
  data[is.na(data)] <- replace
  cat(paste(num_miss," row have been populated by value:",replace,".\n\n"))
  return(data)
}

PopMiss.numeric<-function(data,na.treatment,replace){
  num_miss <- length(which(is.na(data)))
  na.treatment <- match.arg(na.treatment)
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

PopMiss.data.frame<-function(data,na.treatment,replace){
  na.treatment <- match.arg(na.treatment)
  num_miss <- which(sapply(data,anyNA))
  if(isTRUE(na.treatment=="delete")) {
    cat(paste(num_miss," columns have been cleaned.\n\n"))
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
    
    cat(paste(num_miss," columns have been populated with mean or mode.\n\n"))
    return(data)
  }
  else{
    if (is.null(replace)) stop("NULL value used for populating NAs. Chosen \"replace\" method, but didn't provide a value for replace parameter?")
    
    warning("Provided replacement will be used for all missing value.\n\n")
    str_name <- names(data)
    for(i in num_miss)
      data[is.na(data[,str_name[i]]),str_name[i]] <- replace
    cat(paste(num_miss," columns have been populated with mean or mode.\n\n"))
    return(data)
  }
}

PopMiss.matrix <- function(data,na.treatment,replace){
	if(identical(match.arg(na.treatment),"delete")) warning("na.treatment is \"delete\" and 'data' is a matrix. This only works correctly if whole rows are missing")
	matrix(PopMiss(as.vector(data),na.treatment,replace),ncol=ncol(data))
}

setMethod("PopMiss",signature(data="factor"),PopMiss.factor)
setMethod("PopMiss",signature(data="character"),PopMiss.factor)
setMethod("PopMiss",signature(data="Date"),PopMiss.factor)
setMethod("PopMiss",signature(data="logical"),PopMiss.factor)
setMethod("PopMiss",signature(data="integer"),PopMiss.integer)
setMethod("PopMiss",signature(data="numeric"),PopMiss.numeric)
setMethod("PopMiss",signature(data="data.frame"),PopMiss.data.frame)
setMethod("PopMiss",signature(data="matrix"),PopMiss.matrix)

