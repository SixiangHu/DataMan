#' Cramers' V Test
#'
#' @description 
#' Conduct Cramers V test. Please populate or delete missing value before runnign this function.
#' @usage CramersV(x,y=NULL)
#' @param x It could be non-numerical variable, data.frame or matrix.
#' @param y Provided when x is a vector. Will be ignored when x is a data.frame or matrix.
#' @author Sixiang Hu
#' @export
#' @examples
#' 
#' x <- sample(1:5,100,rep=TRUE)
#' y <- sample(1:5,100,rep=TRUE)
#' z <- sample(1:5,100,rep=TRUE)
#' dm <- as.matrix(cbind(x,y,z))
#' CramersV(dm)

CramersV <- function(x,y=NULL){
  if (class(x) == "numeric" && class(x)!="integer") 
    stop("Cramers' V Test is used on nominal or discrete variables.")
  UseMethod("CramersV",x)
}

#' @export
CramersV.default <- function(x,y=NULL){
  if (class(y) == "numeric" && class(y)!="integer") 
    stop("Cramers' V Test is used on nominal or discrete variables.")
  if (is.null(x) || is.null(y))
    stop("When x is integer vector, y must be provided.")
  if (length(x) != length(y))
    stop("x and y must have the same length.")
  if (length(x)<=1 || length(y) <=1 )
    stop("x and y should have more than 2 records,")
  if (class(y)!="integer") y<-as.integer(as.factor(y))
  CramersV_C(x,y)
}

#' @export
CramersV.data.frame <- function(x,y=NULL){
  if(!is.null(y)) 
    warning("y will be ignored.")
  if(dim(x)[1]<2 || dim(x)[2]<2)
    stop("dimension of the data frame should be larger than 1xN or Nx1.")
  
  indCol <- sapply(x,function(xx) xor(is.numeric(xx),is.integer(xx)))
  strDrop <- names(x)[indCol]
  
  if(sum(indCol)>0){
    warning("Found numeric columns, and CramersV test will not be conducted on those variables.")
  }
  dm <- as.matrix(x[,setdiff(names(x),strDrop)])
  
  CramersV_DF(dm)
}

#' @export
CramersV.matrix <- function(x,y=NULL){
  if(!is.null(y)) 
    warning("y will be ignored.")
  if(dim(x)[1]<2 || dim(x)[2]<2)
    stop("dimension of the data frame should be larger than 1xN or Nx1.")
  
  if(xor(is.numeric(x[1,1]),is.integer(x[1,1]))){
    stop("Found numeric columns, and CramersV test will not be conducted on those variables.")
  }
  CramersV_DF(x)
}