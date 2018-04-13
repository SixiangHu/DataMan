#' DataSummary
#'
#' @description function gives summary of the dataset.
#' @usage DataSummary(data,wt=NULL,sparkline=FALSE)
#' @param data This could be data frame or a vector.
#' @param missing list of possible values stand for missing.
#' @param wt For a sampled dataset, you may want to specify the wieght for stats calcualted.  It can be a character which is a column name in the dataset provided, 
#' or integer (numeric) weights vector.
#' @param sparkline logical. If true, a string of level percentage will be generated, which can be used later in `shiny` app with `sparkline` package.
#' @details This function provides a data summary including min, max, number of unique values and number if missing values.
#' The min and max will ignore missing value in the data.  The input should be a `data.frame`.
#' @author Sixiang Hu
#' @importFrom data.table data.table :=
#' @export DataSummary
#' @examples
#' DataSummary(mtcars,missing=list(NA,".","Unknown",-1))

DataSummary <- function(data,missing=list(NA),wt=NULL,sparkline=FALSE){
  UseMethod("DataSummary",data)
}

#' @export
#' @rdname DataSummary
DataSummary.data.frame <- function(data,missing=list(NA),wt=NULL,sparkline=FALSE){
  setDT(data)
  DataSummary.data.table(data,missing,wt,sparkline)
}

#' @export
#' @rdname DataSummary
DataSummary.data.table <- function(data,missing=list(NA),wt=NULL,sparkline=FALSE){
  
  if(is.null(wt)) weight <- rep(1,nrow(data))
  else if (class(wt) == "character") weight <- data[[wt]]
  else if (class(wt) %in% c("integer","numeric") and length(wt) == nrow(data)) weight <- wt
  else stop("DataSummary: wt provided is not in correct format.\n")
  
  nr <- nrow(data)
  dsName    <- names(data)
  
  for (i in dsName) set(data,i=which(data[[i]] %in% missing), j=i, value=NA)
     
  dsClass   <- data[,sapply(.SD,function(x) class(x)[1])]
  dsNLevels <- data[,sapply(.SD,function(x) uniqueN(x))]
  dsMiss    <- data[,sapply(.SD,function(x) sum(is.na(x)))]
  
  dsMean    <- data[,sapply(.SD,function(x){
    if(is.numeric(x) | is.integer(x)) { as.character(round(weighted.mean(x,weight,na.rm = TRUE),6))}
    else {"NA"}
  })

  dsMax    <- data[,sapply(.SD,function(x){
    if(is.numeric(x) | is.integer(x)) as.character(round(max(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp)  | is.na(dsTemp)) dsTemp <- as.character(x.dt[,sum(weight),by=x][order(-V1)][2,list(x)])
      return(dsTemp) 
    }
  })]
  
  dsMin    <- data[,sapply(data,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x,weight)
      dsTemp <- as.character(x.dt[,sum(weight),by=x][order(V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) dsTemp <- as.character(x.dt[,sum(weight),by=x][order(V1)][2,list(x)])
      return(dsTemp)
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
                      "Missing_Pct" = round(dsMiss / nr ,2), 
                      "Mean|Mode"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "%Distribution"=dsStr,
                      stringsAsFactors = FALSE))
  }
  else{
    return(data.frame("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Missing_Pct" = round(dsMiss / nr ,2), 
                      "Mean|Mode"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      stringsAsFactors = FALSE))
  }
}

#global variable
globalVariables(c("V1",".N","f","N",".SD"))
