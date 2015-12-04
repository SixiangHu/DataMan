embAVE <- function(df,A=NULL,E=NULL,Var=NULL,Weight="Weight",by=NULL) {
  arguments <- as.list(match.call())
  
  if ( is.null(df) ) stop("Data frame provided is null.")
  if ( is.null(A) ) stop("Actual variable provided is null.")
  if ( is.null(E) ) stop("Expected variable provided is null.")
  if ( is.null(Var) ) stop("Test variable provided is null.")
  
  name_list <- names(df)
  if ( !Var %in% name_list ) stop("Test varaibel is not in the data frame.")
  if ( Weight %in% name_list ) {
    w <- as.numeric(df[,Weight])
  }
  else {
    w <- rep(1,dim(df)[1])
  }
  
  if (is.character(A)) {
    if (! A %in% name_list) stop("Actual variable name provided is not in the data frame.")
    #else data.table:setnames(df,A,"Act")  # setnames will change the data frame name in the parent env
    else df$Act <- df[,A] * w  
  }
  else {
    df$Act <- A * w 
  }
  
  if (is.character(E)) {
    if (! E %in% name_list) stop("Expected variable name provided is not in the data frame.")
    #else data.table:setnames(df,E,"Exp")
    else df$Exp <- df[,E] * w  
  }
  else {
    df$Exp <- E * w 
  }
  
  if (!is.null(by)){
    if (is.character(by) ) {
      if(length(intersect(by,name_list)) <length(by)) {
        stop("'by' variable names provided cannot be found in the data frame.")
      }
      else bylist <- append(Var,by)
    }
    else bylist <- append(Var,eval(arguments$by))
  }
  
  dt <- data.table::as.data.table(df)
  
  #reshape(data.frame(AvsE[,mean(V1,na.rm=TRUE),by=eval(append(Var,by[1]))]), timevar=by[1],idvar=Var, v.names = "V1",direction = "wide")
  dtr <-as.data.frame(reshape(dt[,lapply(.SD,sum,na.rm=TRUE),by=eval(append(Var,by)),.SDcols = c("Act","Exp")][,AvsE:=Act/Exp], 
                              timevar=by[1],idvar=Var, v.names = "AvsE",direction = "wide",drop=c("Act","Exp")))
  
  rownames(dtr) <- dtr[,1]
  dtr[,Var] <- NULL
  Consistency <- rep(FALSE,dim(dtr)[1])
  
  for(i in 1:dim(dtr)[1])
    Consistency[i] <- (length(which(dtr[i,]>=1))==(dim(dtr)[2] - sum(is.na(dtr[i,])))) | (length(which(dtr[i,]<1))==(dim(dtr)[2] - sum(is.na(dtr[i,]))))
  
  dtr$Consistency <- Consistency
  
  dtr
}