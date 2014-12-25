#' Cramers' V Test
#'
#' @description 
#' Conduct Cramers V test. Please populate or delete missing value before runnign this function.
#' @usage CramersV(data)
#' @param data This could be data frame or matrix.
#' @seealso PopMiss
#' @author Sixiang Hu
#' @examples
#' CramersV(cars)

CramersV <- function(data){
 
  if (length(data)==1) stop("Only one variable in the data.\n")
  
  if (dim(data)[1]<=1) stop("No enough obs in data to conduct Cramers' V test.\n")
  
  varNumber <- dim(data)[2]

  Cramer <- matrix(,nrow=varNumber,ncol=varNumber)
  
  for (i in 1:varNumber){
    x <- data[,i]
    for (j in i:varNumber){
      if (i==j) Cramer[i,j] <- 1
      else {
        y <- data[,j]
        Cramer[i,j] <- cv.test(x,y)
      }
      Cramer[j,i] <- Cramer[i,j]
    }
  }
  
  colnames(Cramer) <- dataNames
  rownames(Cramer) <- dataNames
  return(Cramer)
}

cv.test <- function(x,y) {
  all2int <- function(x){
    if (is.character(x)){
      return(as.integer(factor(x)))
    }
    else if (is.factor(x)){
      return(as.integer(x))
    }
    else return(x)
  }
  
  x <- all2int(x)
  y <- all2int(y)
  
  x_len <- length(unique(x))
  y_len <- length(unique(y))
  
  if (x_len==1 || y_len==1) {
    CV <- 1
    chi <- 0
  }
  else if (x_len==2 && y_len==2) {
    tb <- table(x,y)
    Sx <- as.integer(table(x,useNA ="always"))
    Sy <- as.integer(table(y,useNA ="always"))
    Oxy <- sum(Sx)
    
    chi <- (tb[1,1]*tb[2,2]-tb[1,2]*tb[2,1])*Oxy/(Sx[1]*Sx[2]*Sy[1]*Sy[2])
    CV <- sqrt(chi/(Oxy * min(x_len-1, y_len-1)))
  }
  else {
    tb <- table(x,y,useNA ="always")
    Sx <- as.integer(table(x,useNA ="always"))
    Sy <- as.integer(table(y,useNA ="always"))
    Oxy <- sum(Sx)
    
    chi <- 0
    for (i in 1:x_len){
      for (j in 1:y_len){
        Exy <- Sx[i]*Sy[j]/Oxy
        chi <- chi + (tb[i,j]-Exy)^2/Exy
      }
    }
    CV <- sqrt(chi/(Oxy * min(x_len-1, y_len-1)))
  }
  
  return(as.numeric(CV))
}
