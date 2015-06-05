# #' Cramers' V Test
# #'
# #' @description 
# #' Conduct Cramers V test. Please populate or delete missing value before runnign this function.
# #' @usage CramersV(data)
# #' @param data This could be data frame or matrix.
# #' @seealso PopMiss
# #' @author Sixiang Hu
# #' #'@export
# #' @examples
# #' CramersV(cars)
# 
# CramersV <- function(data){
#   if (is.null(data)) stop("Object is null.\n")
#   
#   if (length(data)==1) stop("Only one variable in the data.\n")
#   
#   if (dim(data)[1]<=1) stop("No enough obs in data to conduct Cramers' V test.\n")
#   
#   dataNames <- names(data)
#   varNumber <- dim(data)[2]
# 
#   Cramer <- matrix(,nrow=varNumber,ncol=varNumber)
#   
#   for (i in 1:varNumber){
#     x <- data[,i]
#     for (j in i:varNumber){
#       if (i==j) Cramer[i,j] <- 1
#       else {
#         y <- data[,j]
#         
#         x_len <- length(unique(x))
#         y_len <- length(unique(y))
#         
#         if (x_len==1 || y_len==1) {
#           CV <- 1
#           chi <- 0
#         }
#         else if (x_len==2 && y_len==2) {
#           tb <- quickTable(x,y)
#           Sx <- quickTable(x)
#           Sy <- quickTable(y)
#           Oxy <- sum(Sx)
#           
#           chi <- (tb[1,1]*tb[2,2]-tb[1,2]*tb[2,1])*Oxy/(Sx[1]*Sx[2]*Sy[1]*Sy[2])
#           V <- sqrt(abs(chi)/(Oxy * min(x_len-1, y_len-1)))
#         }
#         else {
#           tb <- quickTable(x,y)
#           Sx <- quickTable(x)
#           Sy <- quickTable(y)
#           Oxy <- sum(Sx)
#           
#           chi <- 0
#           for (k in 1:x_len){
#             for (l in 1:y_len){
#               Exy <- Sx[k]*Sy[l]/Oxy
#               chi <- chi + (tb[k,l]-Exy)^2/Exy
#             }
#           }
#           CV <- sqrt(chi/(Oxy * min(x_len-1, y_len-1)))
#         }
#         Cramer[i,j] <- CV
#       }
#       Cramer[j,i] <- Cramer[i,j]
#     }
#   }
#   
#   colnames(Cramer) <- dataNames
#   rownames(Cramer) <- dataNames
#   return(Cramer)
# }
# 
# #' quickTable
# #'
# #' @description 
# #' Build cross table on given vector(s). Similar to \code{\link{table}} function in base, but much faster using data.table package.
# #' @usage quickTable(x,y=NULL,exclude = c(NULL,NA))
# #' @param x integer, character, or date vectors.
# #' @param y integer, character, or date vectors.
# #' @param exclude include missing value (NULL) or exclude (NA)
# #' @return return a matrix.
# #' @seealso \code{\link{table}}
# #' @author Sixiang Hu
# #' #'@export quickTable
# #' @importFrom data.table as.data.table
# #' @examples
# #' quickTable(cars$speed,cars$dist)
# 
# quickTable <- function(x,y=NULL,exclude = c(NULL,NA)){
#   x <- as.data.table(x = factor(x,exclude = exclude))
#   if (is.null(y)) {
#     dt_cr <- x[,.N,by=x]
#     dt_tb <- tapply(dt_cr$N,list(dt_cr$x), sum)
#   }
#   else if (!is.null(y)){
#     y <- as.data.table(factor(y,exclude = exclude))
#     dt <- as.data.table(x,y)
#     dt_cr <- dt[,.N,by=list(x,y)]
#     dt_tb <- tapply(dt_cr$N,list(dt_cr$x,dt_cr$y), sum)
#   }
#   
#   return(replace(dt_tb,is.na(dt_tb),0))
# }