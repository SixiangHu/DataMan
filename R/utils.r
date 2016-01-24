#rbokeh tool sets
.tools <- c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save")

#color
.cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
.cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#global variable
globalVariables(c("V1",".N","f","N"))

#return position index and name of a given variable in the data
.VarPosition <- function(data,var){
  tmp <- list(posi=NULL,name=NULL)
  
  if (is.character(var)) {
    if (!var %in% colnames(data)) 
      stop(paste("Variable (",var,") cannot be found.",""))
    
    tmp$posi <- match(var,names(data))
    tmp$name <- var
  }
  else if (is.integer(var)) { 
    if (var > nrow(data))
      stop(paste("Position specified (",var,"): subscript out of bounds.",""))
    
    tmp$posi <- var
    tmp$name <- names(data)[var]
  }
  
  if (is.null(tmp$posi))
    stop ("xvar provided is either a character (variable name) or integer (position of the variable).")
  else tmp
}

#whether a dataset is data frame.
.isDFnull <- function(data){
  ("data.frame" %in% class(data)) && (dim(data)[1]==0 || dim(data)[2]==0)
}

#Mean Data For Fitted Mean
.ModeData <- function(data,weights,base=NULL){
  
  if(!("data.frame" %in% class(data))){
    if(length(data)==0) stop("data set is empty.")
  }
  else{
    if( !.isDFnull(data) ){
      VarName <- names(data)
      iLen <- length(VarName)
    }
    else stop("data set is empty.")
  }
  
  if(is.null(weights)) weights<-rep(1,dim(data)[1])
  
  for(i in 1:iLen) {
    x_dt<-data.table::as.data.table(cbind(x=data[,VarName[i]],weights))
    
    if( sum(c("character","factor") %in% class(data[,VarName[i]]))>0 )
      if (!is.null(base[[VarName[i]]])) data[,VarName[i]] <- as.character(base[[VarName[i]]])
      else data[,VarName[i]] <- as.character(x_dt[,sum(weights),by=x][order(-V1)][1,1,with=FALSE])
    else
      if (!is.null(base[[VarName[i]]])) data[,VarName[i]] <- base[[VarName[i]]]
      else data[,VarName[i]] <- x_dt[,sum(weights),by=x][order(-V1)][1,1,with=FALSE]
  }
  
  return(data)
}

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
