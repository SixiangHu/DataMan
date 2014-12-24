#' Cramers' V Test
#'
#' @description Conduct Cramers V test. Please populate or delete missing value before runnign this function.
#' @usage CramersV(data)
#' @param data This could be data frame, matrix or a vector.
#' @keywords Cramers' V
#' @export A data frame or vector with Cramers V statistics
#' @seealso PopMiss
#' @author Sixiang Hu
#' @examples
#' a <- c(sample(LETTERS,5),NA)
#' PopMiss(a)

CramersV <- function(data){
  all2int <- function(x){
    if (is.character(x)){
      return(as.integer(factor(x)))
    }
    else if (is.factor(x)){
      return(as.integer(x))
    }
    else return(x)
  }
  
  data4stat <- sapply(data,all2int)
    
  if (length(data4stat)==1) {
    print("Only one variable in the data.\n")
    return(1)
  }
  
  dataNames <- names(data4stat)
  varNumber <- dim(data4stat)[2]

  Cramer <- matrix(,nrow=varNumber,ncol=varNumber)
  
  for (i in 1:varNumber){
    x <- data4stat[,i]
    names(x) <- dataNames[i]
    for (j in i:varNumber){
      if (i==j) Cramer[i,j] <- 1
      else {
        y <- data4stat[,j]
        names(y) <- dataNames[j]
        Cramer[i,j] <- cv.test(x,y)
      }
      
      Cramer[j,i] <- Cramer[i,j]
    }
  }
  
  colnames(Cramer) <- dataNames
  rownames(Cramer) <- dataNames
  return(Cramer)
}


cv.test2 <- function(x,y) {
  x_len <- length(unique(x))
  y_len <- length(unique(y))
  if (x_len==1 || y_len==1) CV <- 1
  else CV <- sqrt(chisq.test(x, y, correct=FALSE)$statistic /
                    (length(x) * (min(x_len,y_len) - 1)))
  #print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

cv.test <- function(x,y) {
  x_len <- length(unique(x))
  y_len <- length(unique(y))
  
  if (x_len==1 || y_len==1) {
    CV <- 1
    chi <- 0
  }
  else if (x_len==2 && y_len==2) {
    tb <- table(x,y)
    Sx <- as.integer(table(x))
    Sy <- as.integer(table(y))
    Oxy <- sum(Sx)
    
    chi <- (tb[1,1]*tb[2,2]-tb[1,2]*tb[2,1])*Oxy/(Sx[1]*Sx[2]*Sy[1]*Sy[2])
    CV <- sqrt(chi/(Oxy * min(x_len-1, y_len-1)))
  }
  else {
    tb <- table(x,y,useNA ="always")
    Sx <- as.integer(table(x))
    Sy <- as.integer(table(y))
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
  
  #print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}
