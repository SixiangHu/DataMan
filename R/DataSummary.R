#' DataSummary
#'
#' @description function gives summary of the dataset.
#' @usage DataSummary(data,missing=list(NA),wt=NULL,entropy=FALSE,
#' entropy_method="emp",sparkline=FALSE)
#' @param data This could be data frame, a vector, matrix or data.table.
#' @param missing list of possible missing values.
#' @param wt For a sampled dataset, you may want to specify the wieght for those stats calcualted.  
#'     It can be a character which is a column name in the dataset provided, or integer (numeric) weights vector.
#' @param entropy logical. whether to include entropy as part of output. It will take quite a long time.
#' @param entropy_method The name of the entropy estimator. 
#'     The `infotheo` package implements four estimators : "emp", "mm", "shrink", "sg" (default:"emp").
#' @param sparkline logical. If true, a string of level percentage will be generated, 
#'     which can be used later in `shiny` app with `sparkline` package.
#' @details This function provides a data summary including min, max, number of unique values and number if missing values.
#'     The min and max will ignore missing value in the data.
#'     As for the mean value, if it is character variable the mode (most frequency level) will be recorded.
#' @author Sixiang Hu
#' @importFrom data.table data.table := setDT uniqueN
#' @importFrom infotheo entropy discretize
#' @importFrom stats weighted.mean
#' @importFrom checkmate testCharacter testNumeric
#' @export DataSummary
#' @examples
#' DataSummary(mtcars,missing=list(NA,".","Unknown",-1))

DataSummary <- function(data,missing=list(NA,NaN,Inf),wt=NULL,entropy=FALSE,entropy_method="emp",sparkline=FALSE){
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
  
  if(is.null(wt)) weight <- rep(1,nrow(data))
  else if ( testCharacter(wt) ) weight <- data[[wt]]
  else if ( testNumeric(wt) &&  length(wt) == nrow(data)) weight <- wt
  else stop("DataSummary: wt provided is not in correct format.\n")
  
  nr <- nrow(data)
  dsName    <- names(data)
  
  for (i in dsName) set(data,i=which(data[[i]] %in% missing), j=i, value=NA)
     
  dsClass   <- data[,sapply(.SD,function(x) class(x)[1])]
  dsNLevels <- data[,sapply(.SD,function(x) uniqueN(x,na.rm=TRUE))]
  dsMiss    <- data[,sapply(.SD,function(x) sum(is.na(x)))]

  dsMaxMin    <- data[,sapply(.SD,function(x,weight){
    if(testNumeric(x)) {
      dsTempMax <- as.character(round(max(x,na.rm = TRUE),6))
      dsTempMin <- as.character(round(min(x,na.rm = TRUE),6))
    }else {
      x.dt<- data.table(x,weight)
      dsFreq <- x.dt[,sum(weight),by=x][order(-V1)]
      dsTempMax <- as.character(dsFreq[1,list(x)])
      dsTempMin <- as.character(dsFreq[nrow(dsFreq),list(x)])
      if(dsTempMax=="NA" || is.na(dsTempMax)) dsTempMax <- as.character(dsFreq[2,list(x)])
      if(dsTempMin=="NA" || is.na(dsTempMin)) dsTempMin <- as.character(dsFreq[nrow(dsFreq)-1,list(x)])
    }
    return(paste0(dsTempMax,"|DM|",dsTempMin))
  },weight=weight)]
  
  dsMean    <- data[,sapply(.SD,function(x,weight){
    if( testNumeric(x) ) { as.character(round(sum(x*weight,na.rm = TRUE)/sum(weight*(!is.na(x))),6))}
    else {"NA"}
  },weight=weight)]
  
  dsMax <- gsub("(.*)(\\|DM\\|)(.*)","\\1",dsMaxMin)
  dsMin <- gsub("(.*)(\\|DM\\|)(.*)","\\3",dsMaxMin)
  dsMean <- ifelse(is.na(dsMean) | dsMean=="NA",dsMax,dsMean)
  
  dsEntropy <- rep(0,length(dsName))
  if(entropy) dsEntropy <- data[,sapply(data,function(x) entropy(discretize(x), method = entropy_method))]

  if(sparkline) {
    dsStr <- data[,sapply(data,function(x) {
      freq <- data.table(x)[,.N,by=x][order(x)][,f:=N/sum(N)]
      if (nrow(freq)>=100) return("More than 100 levels")
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
                      "%Distribution"=dsStr))
  }else{
    return(data.table("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Missing_pct"=round(dsMiss/nr,2),
                      "Mean"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "Entropy"=round(dsEntropy,6)))
  }
}
