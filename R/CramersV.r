#' Cramers' V Test
#'
#' @description 
#' Conduct Cramers V test. Please populate or delete missing value before runnign this function.
#' @usage CramersV(x,y=NULL,Bias_Cor=FALSE)
#' @param x It could be non-numerical variable, data.frame or matrix.
#' @param y Provided when x is a vector. Will be ignored when x is a data.frame or matrix.
#' @param Bias_Cor logic. Whether a bias correction is needed.
#' @author Sixiang Hu
#' @export
#' @examples
#' 
#' x <- sample(1:5,100,rep=TRUE)
#' y <- sample(1:5,100,rep=TRUE)
#' z <- sample(1:5,100,rep=TRUE)
#' dm <- as.matrix(cbind(x,y,z))
#' CramersV(dm)

CramersV <- function(x,y=NULL,Bias_Cor=FALSE){
  if (is.double(x)) stop("Cramers' V Test is used on nominal or discrete variables.")
  UseMethod("CramersV",x)
}

#' @export
#' @rdname CramersV
CramersV.default <- function(x,y=NULL,Bias_Cor=FALSE){
  if (is.double(x) || is.double(y)) stop("Cramers' V Test is used on nominal or discrete variables.")
  if (is.null(x) || is.null(y))
    stop("When x is integer vector, y must be provided.")
  if (length(x) != length(y))
    stop("x and y must have the same length.")
  if (length(x)<=1 || length(y) <=1 )
    stop("x and y should have more than 2 records,")
  if (class(y)!="integer") y<-as.integer(as.factor(y))
  CramersV_C(x,y,Bias_Cor)
}

#' @export
#' @rdname CramersV
CramersV.data.frame <- function(x,y=NULL,Bias_Cor=FALSE){
  if(!is.null(y)) 
    warning("y will be ignored.")
  if(dim(x)[1]<2 || dim(x)[2]<2)
    stop("dimension of the data frame should be larger than 1xN or Nx1.")
  
  indCol <- sapply(x,function(xx) !is.double(xx))
  strDrop <- colnames(x)[indCol]
  
  if( sum(indCol)<dim(x)[2] ){
    warning("Found numeric columns, and CramersV test will not be conducted on those variables.")
  }
  else if( sum(indCol)==0 ) {
    stop("All columns are numerical. No CramersV will be calculated.")
  }
  
  dm <- sapply(x[,strDrop],function(xx)
  {
    if(is.character(xx)){ as.integer(as.factor(xx)) }
    else if (is.factor(xx)) { as.integer(xx)}
    else xx
  })
  
  CramersV_DF(dm,Bias_Cor)
}

#' @export
#' @rdname CramersV
CramersV.matrix <- function(x,y=NULL,Bias_Cor=FALSE){
  if(!is.null(y)) warning("y will be ignored.")
  CramersV.data.frame(as.data.frame(x,stringsAsFactors = TRUE),Bias_Cor=Bias_Cor)
}