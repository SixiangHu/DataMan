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

setGeneric("DetMiss",function(data) standardGeneric("DetMiss"))

DetMiss.matrix <- function(data){
	v <- DetMiss(as.vector(data))
}

DetMiss.factor<-function(data){
  if(length(data)==0) stop(paste("object '",deparse(substitute(data)),"' blank or not found.\n"))
  RetTab<-data.frame(class(data),sum(is.na(data)),stringsAsFactors=FALSE)
  colnames(RetTab) <- c("    Variable Class","    Num of NA")
  return(RetTab)
}

DetMiss.data.frame<-function(data){
  
  RetTab<-data.frame()
  
  if(dim(data)[1]==0) stop(paste("object '",deparse(substitute(data)),"' blank or not found.\n"))
  
  AllMiss <- which(sapply(data,anyNA))
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

setMethod("DetMiss",signature(data="factor"),DetMiss.factor,valueClass="factor")
setMethod("DetMiss",signature(data="logical"),DetMiss.factor,valueClass="logical")
setMethod("DetMiss",signature(data="Date"),DetMiss.factor,valueClass="Date")
setMethod("DetMiss",signature(data="character"),DetMiss.factor,valueClass="character")
setMethod("DetMiss",signature(data="integer"),DetMiss.factor,valueClass="integer")
setMethod("DetMiss",signature(data="numeric"),DetMiss.factor,valueClass="numeric")
setMethod("DetMiss",signature(data="data.frame"),DetMiss.data.frame,valueClass="data.frame")
setMethod("DetMiss",signature(data="matrix"),DetMiss.matrix,valueClass="numeric")


anyNA <- function(x){
  if(getRversion() < "3.1.0") any(is.na(x))
  else anyNA(x)
}
