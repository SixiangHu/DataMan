#' dataComp
#' 
#' @description This function judges whether a dataset has a profile changes.
#' @usage dataComp(old,new,alpha=0.01,obs.lim=5)
#' @param old data frame or vector. It holds the original data or data you are expecting.
#' @param new data frame or vector. It holds the data you want to test.
#' @param alpha significant level. By defaul, is 0.01.
#' @param obs.lim integer. To specify the a limit that factor levels with no more
#' than this integer will not be tested.
#' @details A string vector will be return indicating factors that have a profile change 
#' if both old and new are data frame. If old and new are vector, then TRUE / FALSE will be returned.
#' 
#' Currently, this function will not test those levels with less than 5 obs.
#' 
#' A fisher exact test would be more suitable.
#' @return This function will return a data.frame if inputs are data.frame.
#' `NULL`     - means there are no levels have more than obs.lim, hence Chi-square test is not conducted.
#' `NA`       - p-value is `NA`, because some levels have 0 observations.
#' `Nochange` - cannot reject the null hypothesis that there is no change.
#' `Changed`  - reject the null hypothesis that there is no change.
#' @author Sixiang Hu
#' @importFrom stats chisq.test
#' @export dataComp
#' @examples
#' 
#' x <- mtcars
#' y <- rbind(mtcars,mtcars)
#' y$am[1:10] <- 1
#' dataComp(x,y)

dataComp <- function(old,new,alpha=0.01,obs.lim=5){
  if(is.data.frame(old)){
    
    if(.isDFnull(old)) stop("No expectation data given.")
    if(.isDFnull(new)) stop("No test data given.")
    
    strName <- colnames(old)
    res <- data.frame(Name=character(),Result=character())
    
    for (i in strName){
      if (i %in% colnames(new)){
        x <- varComp(old[,i],new[,i],alpha,obs.lim)
        if (is.null(x)) {
          tmp <- data.frame(Name=i,Result="NULL")
          res <- rbind(res,tmp)
        }
        else if (is.na(x$p.value)) {
          tmp <- data.frame(Name=i,Result="NA")
          res <- rbind(res,tmp)
        }
        else if (x$p.value < alpha) {
          tmp <- data.frame(Name=i,Result="Changed")
          res <- rbind(res,tmp)
        }
      }
      else{
        tmp <- data.frame(Name=i,Result="NotInNew")
        res <- rbind(res,tmp)
      }
    }
  }
  else {
    
    if(is.null(old)) stop("No expected data given.")
    if(is.null(new)) stop("No test data given.")
    
    if(is.data.frame(new)) stop("new dataset must be a vector if a vector has provided as expected.")
    x <- varComp(old,new,alpha,obs.lim)
    if (is.null(x)) res <- "NULL"
    else if (is.na(x$p.value)) res <- "NA"
    else if (x$p.value < alpha) res <- "Changed"
    else "Nochange"
  }
  
  res
}

#' @export varComp
#' @rdname dataComp
varComp <- function(old,new,alpha=0.05,obs.lim){
  if(is.null(old)) stop("Expected data is not given.")
  if(is.null(new)) stop("Test data is not given.")

  df_o <- as.data.frame(table(old,useNA="no"))
  df_n <- as.data.frame(table(new,useNA="no"))
  df <- merge(x = df_o,y=df_n,by.x="old",by.y="new",all = FALSE)

  #get rid of any NA generated in merging
  df <- DataMan::PopMiss(df,na.treatment = "delete")
   
  #Chisq is not suitable for obs less than 5
  df <- subset(df,Freq.x>obs.lim && Freq.y>obs.lim)
  
  if (.isDFnull(df)) return(NULL)
  else return(suppressWarnings(chisq.test(df[,2],p=df[,3]/sum(df[,3]))))
}
