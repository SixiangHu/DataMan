#' DataSummary
#'
#' @description This function allows you to have a summary about the given dataset.
#' @usage DataSummary(data,wt=NULL,sparkline=FALSE)
#' @param data This could be data frame or a vector.
#' @param wt For a sampled dataset, you may want to specify the wieght for stats calcualted.  It can be a character which is a column name in the dataset provided, 
#' or integer (numeric) weights vector.
#' @param sparkline logical. If true, a string of level percentage will be generated, which can be used later in `shiny` app with `sparkline` package.
#' @details This function provides a data summary including min, max, number of unique values and number if missing values.
#' The min and max will ignore missing value in the data.  The input should be a `data.frame`.
#' @author Sixiang Hu
#' @importFrom data.table data.table :=
#' @export DataSummary
#' @examples
#' DataSummary(cars)

DataSummary <- function(data,wt=NULL,sparkline=FALSE){
  UseMethod("DataSummary",data)
}

#' @export
#' @rdname DataSummary
DataSummary.data.frame <- function(data,wt=NULL,sparkline=FALSE){

  if(is.null(wt)) weight <- rep(1,nrow(data))
  else if (class(wt) == "character") weight <- data[,wt]
  else if (class(wt) %in% c("integer","numeric")) weight <- wt
  else stop("DataSummary: wt provided is not in correct format.\n")
  
  dsName    <- names(data)
  dsClass   <- sapply(data,function(x) ifelse(length(class(x))>1,class(x)[1],class(x)))
  dsNLevels <- sapply(data,function(x) nlevels(as.factor(x)))
  dsMiss    <- sapply(data,function(x) sum(is.na(x)))
  
  dsMean    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(weighted.mean(x,weight,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,sum(weight),by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
  })
  
  dsMax    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(max(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,sum(weight),by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
  })
  
  dsMin    <- sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,sum(weight),by=x][order(V1)][2,list(x)])
      else dsTemp
    }
  })
  
  if (sparkline) {
    dsStr <- sapply(data,function(x) {
      freq <- data.table::data.table(x)[,.N,by=x][order(x)][,f:=N/sum(N)]
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

#' @export
#' @rdname DataSummary
DataSummary.data.table <- function(data,wt=NULL,sparkline=FALSE){
  
  if(is.null(wt)) weight <- rep(1,nrow(data))
  else if (class(wt) == "character") weight <- data[,wt]
  else if (class(wt) %in% c("integer","numeric")) weight <- wt
  else stop("DataSummary: wt provided is not in correct format.\n")
  
  dsName    <- names(data) 
  dsClass   <- data[,sapply(.SD,function(x) ifelse(length(class(x))>1,class(x)[1],class(x)))]
  dsNLevels <- data[,sapply(.SD,function(x) nlevels(as.factor(x)))]
  dsMiss    <- data[,sapply(.SD,function(x) sum(is.na(x)))]
  
  dsMean    <- data[,sapply(.SD,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(weighted.mean(x,weight,na.rm = TRUE),6))
    else {
      x.dt<- data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,sum(weight),by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
    })]

  dsMax    <- data[,sapply(.SD,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(max(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,sum(weight),by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
  })]
  
  dsMin    <- data[,sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,sum(weight),by=x][order(V1)][2,list(x)])
      else dsTemp
    }
  })]
  
  if (sparkline) {
    dsStr <- data[,sapply(data,function(x) {
      freq <- data.table::data.table(x)[,.N,by=x][order(x)][,f:=N/sum(N)]
      if (dim(freq)[1]>=100) return("More than 100 levels")
      else return(paste0(round(freq[,f],3),collapse= ","))
    })]
    
    return(data.frame("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Mean|Mode"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "%Distribution"=dsStr))
  }
  else{
    return(data.frame("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Mean|Mode"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax))
  }
}

#global variable
globalVariables(c("V1",".N","f","N",".SD"))
