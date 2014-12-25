#' DataSummary
#'
#' @description This function allows you to have a summary about the given dataset.
#' @usage DataSummary(data,weights=NULL)
#' @param data This could be data frame or a vector.
#' @author Sixiang Hu
#' @seealso PopMiss
#' @examples
#' DataSummary(cars)

DataSummary <- function(data,weights=NULL){
  if(is.null(weights)) weights <- rep(1,nrow(data))
  
  dsName    <- names(data)
  dsClass   <- sapply(data,class)
  dsNLevels <- sapply(data,function(x) nlevels(as.factor(x)))
  dsMiss    <- sapply(data,function(x) sum(is.na(x)))
  
  dsMean    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(weighted.mean(x,weights,na.rm = TRUE),6))
    else {
      x.dt<-data.table(x,weights)
      as.character(x.dt[,sum(weights),by=x][order(-V1)][1,list(x)])
    }
  }
  )
  dsMax    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(max(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table(x,weights)
      as.character(x.dt[,sum(weights),by=x][order(-V1)][1,list(x)])
    }
  }
  )
  dsMin    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table(x,weights)
      as.character(x.dt[,sum(weights),by=x][order(V1)][1,list(x)])
    }
  }
  )
  
  return(data.frame("Variable Name"=dsName,
                    "Variable Type"=dsClass,
                    "Number of Unique Value"=dsNLevels,
                    "Number of Missing value"=dsMiss,
                    "Mean or Mode"=unlist(dsMean),
                    "Min"=unlist(dsMin),
                    "Max"=unlist(dsMax)
  )
  )
}