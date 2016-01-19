#' multiParts
#' 
#' @description This function splits a vector into several parts depends on the proportion provided.
#' @usage multiParts(y,p)
#' @param y vector. to be splited.
#' @param p numeric vector. The proportion of each parts.
#' @details By calling `sample` function with `replace=FALSE`, the provided vector can be splited into
#' different proportion which follows the proportion provided.  Row number will be returned for each
#' proportion in list.
#' @author Sixiang Hu
#' @export multiParts
#' @examples
#' 
#' multiParts(1:100,c(0.25,0.25,0.5))

multiParts <- function(y,p){
  if(is.null(y)) stop("no inputs")
  
  if(sum(p)<1) warning("Sum of proportion is less than 1, 
                       rest of the data will be dropped.")
  
  if(sum(p)>1) stop("Sum of the proportion is larger than 1.")
  
  rn <- 1:length(y)
  np <- length(p)
  
  res <- list()
  #   Use sample function instead
  for (i in 1:np){
    ind <- sample(1:length(rn),size=floor(p[i]*length(rn)))
    res[[i]] <- rn[ind]
    rn <- rn[-ind]
    p <- p/(sum(p[-c(1:i)]))
  }  
  res
}