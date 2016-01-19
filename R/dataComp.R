#' dataComp
#' 
#' @description This function judges whether a dataset has a profile changes.
#' @usage dataComp(old,new,alpha=0.05)
#' @param old data frame or vector. It holds the original data or data you are expecting.
#' @param new data frame or vector. It holds the data you want to test.
#' @param alpha significant level. By defaul, is 0.05.
#' @details A string vector will be return indicating factors that have a profile change 
#' if both old and new are data frame. If old and new are vector, then TRUE / FALSE will be returned.
#' 
#' This method is not robust when frequency of a factor level i sless than 5. 
#' A fisher exact test would be more suitable.
#' @author Sixiang Hu
#' @export dataComp
#' @examples
#' 
#' x <- mtcars
#' y <- rbind(mtcars,mtcars)
#' y$am[1:10] <- 1
#' dataComp(x,y)

dataComp <- function(old,new,alpha=0.05){
  
  if(is.null(old)) stop("No expectation data given.")
  if(is.null(new)) stop("No test data given.")
  
  res <- NULL
  
  if(is.data.frame(old)){
    strName <- names(old)

    for (i in strName){
      if (i %in% names(new)){
        suppressWarnings(x<- chisq.test(table(old[,i]),p=table(new[,i])/sum(table(new[,i]))))
        if (x$p.value < alpha) res <- c(res,i)
      }
    }
  }
  else {
    if(is.data.frame(new)) stop("new dataset must be a vector if a vector has provided as expected.")
    suppressWarnings(x<- chisq.test(table(old),p=table(new)/sum(table(new))))
    if (x$p.value < alpha) res <- TRUE
  }
  
  if(is.null(res)) print("no changes found.")
  else res
}