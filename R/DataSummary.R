DataSummary <- function(data,missing=list(NA),sparkline=FALSE,entropy_methods = "emp"){
  for(j in names(data))
    set(data,i=which(data[[j]] %in% missing), j=j, value=NA)
  
  dsName    <- names(data) 
  dsClass   <- data[,sapply(.SD,function(x) ifelse(length(class(x))>1,class(x)[1],class(x)))]
  dsNLevels <- data[,sapply(.SD,function(x) length(unique(x)))]
  dsMiss    <- data[,sapply(.SD,function(x) sum(is.na(x)))]

  dsMean    <- data[,sapply(.SD,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(mean(x,na.rm = TRUE),6))
    else {
      x.dt<- data.table::data.table(x)
      dsTemp <- as.character(x.dt[,V1:=.N,by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) || is.na(dsTemp)) as.character(x.dt[,V1:=.N,by=x][order(-V1)][2,list(x)])
      else dsTemp
    }
  })]
  
  dsMax    <- data[,sapply(.SD,function(x){
    if(is.numeric(x) || is.integer(x)) as.character(round(max(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x)
      dsTemp <- as.character(x.dt[,.(V1=.N),by=x][order(-V1)][1,list(x)])
      if(is.null(dsTemp) | is.na(dsTemp)) dsTemp <- as.character(x.dt[,.(V1=.N),by=x][order(-V1)][2,list(x)])
      return(dsTemp)
    }
  })]
  
  dsMin    <- data[,sapply(data,function(x){
    if(is.numeric(x) | is.integer(x)) as.character(round(min(x,na.rm = TRUE),6))
    else {
      x.dt<-data.table::data.table(x)
      dsTemp <- as.character(x.dt[,.(V1=.N),by=x][order(V1)][1,list(x)])
      if(is.null(dsTemp) | is.na(dsTemp)) dsTemp <- as.character(x.dt[,.(V1=.N),by=x][order(V1)][2,list(x)])
      return(dsTemp)
    }
  })]
  
  dsEntrophy <- data[,sapply(data,function(x){
    infotheo::entropy(discretize(x), method = entropy_methods)
  })]
  
  dsIG <- dsEntrophy*dsNLevels/nrow(data)
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
                      "Missing_pct"=round(dsMiss/nrow(data),2),
                      "Mean|Mode"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "Entrophy"=round(dsEntrophy,6),
                      "%Distribution"=dsStr))
  }
  else{
    return(data.table("VarName"=dsName,
                      "VarType"=dsClass,
                      "Unique"=dsNLevels,
                      "Missing"=dsMiss,
                      "Missing_pct"=round(dsMiss/nrow(data),2),
                      "Mean|Mode"=dsMean,
                      "Min"=dsMin,
                      "Max"=dsMax,
                      "Entrophy"=round(dsEntrophy,6)))
  }
}