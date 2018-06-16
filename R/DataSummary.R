#' DataSummary
#'
#' @description function gives summary of the dataset.
#' @usage DataSummary(data,missing=list(NA),wt=NULL,entropy=FALSE,
#' entropy_method="emp",sparkline=FALSE)
#' @param data This could be data frame or a vector.
#' @param missing list of possible values stand for missing.
#' @param wt For a sampled dataset, you may want to specify the wieght for stats calcualted.  
#' It can be a character which is a column name in the dataset provided, or integer (numeric) weights vector.
#' @param entropy logical. whether to include entropy as part of output. It will take quite a lot of time.
#' @param entropy_method The name of the entropy estimator. 
#' The `infotheo` package implements four estimators : "emp", "mm", "shrink", "sg" (default:"emp").
#' @param sparkline logical. If true, a string of level percentage will be generated, 
#' which can be used later in `shiny` app with `sparkline` package.
#' @details This function provides a data summary including min, max, number of unique values and number if missing values.
#' The min and max will ignore missing value in the data.  The input should be a `data.frame`.
#' @author Sixiang Hu
#' @importFrom data.table data.table := setDT
#' @importFrom infotheo entropy discretize
#' @importFrom stats weighted.mean
#' @export DataSummary
#' @examples
#' DataSummary(mtcars,missing=list(NA,".","Unknown",-1))

DataSummary <- function(data,missing=list(NA),wt=NULL,entropy=FALSE,entropy_method="emp",sparkline=FALSE){
  UseMethod("DataSummary",data)
}

#' @export
#' @rdname DataSummary
DataSummary.data.frame <- function(data,missing=list(NA),wt=NULL,
                                   entropy=FALSE,entropy_method="emp",sparkline=FALSE){
  setDT(data)
  DataSummary.data.table(data,missing,wt,entropy,entropy_method,sparkline)
}

#' @export
#' @rdname DataSummary
#' 
DataSummary.data.table <- function(data,missing=list(NA),wt=NULL,
                                   entropy=FALSE,entropy_method="emp",sparkline=FALSE){
  
  if (is.null(wt)) weight <- rep(1,nrow(data))
  else if ( is.character(wt) ) weight <- data[[wt]]
  else if ( is.numeric(wt) &&  length(wt) == nrow(data)) weight <- wt
  else stop("DataSummary: wt provided is not in correct format.\n")
  
  nr <- nrow(data)
  dsName    <- names(data)
  
  for (i in dsName) set(data,i=which(data[[i]] %in% missing), j=i, value=NA)
     
  dsClass   <- data[,sapply(.SD,function(x) class(x)[1])]
  dsNLevels <- data[,sapply(.SD,function(x) data.table::uniqueN(x))]
  dsMiss    <- data[,sapply(.SD,function(x) sum(is.na(x)))]

  dsMean    <- data[,sapply(.SD,function(x){
    if( is.numeric(x) ) { as.character(round(weighted.mean(x,weight,na.rm = TRUE),6))}
    else {"NA"}
  })]

  dsMax    <- data[,sapply(.SD,function(x){
    if(is.numeric(x)) {as.character(round(max(x,na.rm = TRUE),6))}
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][2,list(x)])
      return(dsTemp)
    }
  })]
  
  dsMin    <- data[,sapply(data,function(x){
    if(is.numeric(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) dsTemp <- as.character(x.dt[,sum(weight),by=x][order(V1)][2,list(x)])
      return(dsTemp)
    }
  })]
  
  dsEntropy <- rep(0,length(dsName))
  if(entropy) dsEntropy <- data[,sapply(data,function(x) infotheo::entropy(infotheo::discretize(x), method = entropy_method))]

  if (sparkline) {
    dsStr <- data[,sapply(data,function(x) {
      freq <- data.table::data.table(x)[,.N,by=x][order(x)][,f:=N/sum(N)]
      if (dim(freq)[1]>=100) return("More than 100 levels")
      else return(paste0(round(freq[,f],3),collapse= ","))
    })]
    
    return(data.table("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Missing_pct"=round(dsMiss/nr,2),
                      "Mean"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "Entropy"=round(dsEntropy,6),
                      "%Distribution"=dsStr,
                      stringsAsFactors = FALSE))
  }else{
    return(data.table("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Missing_pct"=round(dsMiss/nr,2),
                      "Mean"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "Entropy"=round(dsEntropy,6),
                      stringsAsFactors = FALSE))
  }
}