#global variable
globalVariables(c("V1",".N","f","N","Freq.x","Freq.y","value","xvar","variable","freq"))

#' dmBreak
#' @description Give breaks for pretty plot
#' @usage dmBreak(x,n,method="pretty")
#' @param x a vector that needs to break down
#' @param n number of groups
#' @param method Either "pretty" or "equal".
#' @author Sixiang Hu
#' @export dmBreak
dmBreak <- function(x,n,method="pretty"){
  if(is.null(x)) stop("Vector provided is null.")
  if(length(unique(x))<=n) return(x)
  
  x <- x[!is.na(x)]
  method <- match.arg(method,c("pretty","equal"))
  
  if(method == "pretty") 
    return(pretty(x,n))
  else if(method == "equal") 
    return(seq(min(x, na.rm = TRUE),max(x, na.rm = TRUE),length.out=n))
}
