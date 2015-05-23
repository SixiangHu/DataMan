#' DataSummary
#'
#' @description This function allows you to have a summary about the given dataset.
#' @usage DataSummary(data,weights=NULL)
#' @param data This could be data frame or a vector.
#' @param weights For a sampled dataset, you may want to specify the wieght.  This is a vector that each element in the vector giving a weight to the current observation.
#' @param sparkline logical. If true, a string of level percentage will be generated, which can be used later in shiny with sparkline package.
#' @details This function provides a data summary including min, max, number of unique values and number if missing values.
#' The min and max will ignore missing value in the data.  The input should be a `data.frame`.
#' @author Sixiang Hu
#' @export
#' @examples
#' DataSummary(cars)

DataSummary <- function(data,weights=NULL,sparkline=FALSE){

  if(is.null(weights)) weights <- rep(1,nrow(data))
  
  dsName    <- names(data)
  dsClass   <- sapply(data,function(x) ifelse(length(class(x))>1,class(x)[1],class(x)))
  dsNLevels <- sapply(data,function(x) nlevels(as.factor(x)))
  dsMiss    <- sapply(data,function(x) sum(is.na(x)))
  
  dsMean    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(weighted.mean(x,weights,na.rm = TRUE),6))
    else {
      x.dt<-data.table(x,weights)
      dsTemp <- as.character(x.dt[,sum(weights),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp)) as.character(x.dt[,sum(weights),by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
  })
  
  dsMax    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(max(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table(x,weights)
      dsTemp <- as.character(x.dt[,sum(weights),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp)) as.character(x.dt[,sum(weights),by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
  })
  
  dsMin    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table(x,weights)
      as.character(x.dt[,sum(weights),by=x][order(V1)][1,list(x)])
    }
  })
  
  if (sparkline) {
    dsStr <- sapply(data,function(x) {
      freq <- data.table(x)[,.N,by=x][order(x)][,f:=N/sum(N)]
      if (dim(freq)[1]>=100) return("More than 100 levels")
      else return(paste0(round(freq[,f],3),collapse= ","))
    })
    
    return(data.frame("VarName"=dsName,
                      "VarType"=unlist(dsClass),
                      "Unique"=unlist(dsNLevels),
                      "Missing"=unlist(dsMiss),
                      "Mean|Mode"=unlist(dsMean),
                      "Min"=unlist(dsMin),
                      "Max"=unlist(dsMax),
                      "%Distribution"=unlist(dsStr)))
  }
  else{
    return(data.frame("VarName"=dsName,
                      "VarType"=unlist(dsClass),
                      "Unique"=unlist(dsNLevels),
                      "Missing"=unlist(dsMiss),
                      "Mean|Mode"=unlist(dsMean),
                      "Min"=unlist(dsMin),
                      "Max"=unlist(dsMax)))
  }
}