#' Detect Missing Value
#'
#' @description This function allows you to detect missing values in given dataset. 
#' @usage DetMiss(data)
#' @param data This could be data frame, matrix or a vector.
#' @author Sixiang Hu
#' @seealso PopMiss
#' @export
#' @examples
#' a <- c(sample(LETTERS,5),NA)
#' DetMiss(a)

DetMiss <- function(data){
  UseMethod("DetMiss",data)
}

#' @export
DetMiss.matrix <- function(data){
	v <- DetMiss(as.vector(data))
}

#' @export
DetMiss.default<-function(data){
  if(length(data)==0) stop(paste("object '",deparse(substitute(data)),"' blank or not found.\n"))
  RetTab<-data.frame(class(data),sum(is.na(data)),stringsAsFactors=FALSE)
  colnames(RetTab) <- c("    Variable Class","    Num of NA")
  return(RetTab)
}

#' @export
DetMiss.data.frame<-function(data){
  
  RetTab<-data.frame()
  
  if(dim(data)[1]==0) stop(paste("object '",deparse(substitute(data)),"' blank or not found.\n"))
  
  AllMiss <- which(sapply(data,function(x) any(is.na(x))))
  Names <- names(AllMiss)
  iLen <- length(Names)

  if(iLen==0 ){
    cat("No missing value found.\n")
    return()
  }
  
  for(i in 1:iLen){
    var_lng<-length(data[is.na(data[,Names[i]]),Names[i]])
    tmp<-data.frame(AllMiss[[i]],Names[i],class(data[,Names[i]]),var_lng,stringsAsFactors=FALSE)
    RetTab<-rbind(RetTab,tmp)
  }

  colnames(RetTab) <- c("Variable Column","    Variable Name","    Variable Class","    Num of NA")
  RetTab <- RetTab[order(RetTab[,"    Num of NA"],rev(RetTab[,"    Variable Name"]),decreasing=TRUE),]
  return(RetTab)

}