#' Using the dynamic approach for the knapsack problem
#'
#' @name knapsack_dynamic
#'
#' @param x Data frame with two variables v and w
#' @param W knapsack size
#' @return maximum knapsack value and which elements 
#' @export
knapsack_dynamic <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot((W > 0) == TRUE)
  stopifnot(colnames(x) == c("w","v"))
  
  
  n <- nrow(x)
  m <- matrix(ncol = W+1, nrow=n+1)
  m[1,] <- rep(0, W+1)
  w<- x$w
  v <- x$v
  
  for(i in 1:n){
    for(j in 0:W){
      if (w[i] > j){
        m[i+1, j+1] <- m[i, j+1]
      }
      else{ m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-w[i]]+v[i])}
    }
  }
  
  #looking from the back
  j <- j+1
  i <- which.max(m[,j])
  elements<-length(n)
  k <- 1
  elements[k] <- i-1
  
  while(m[i,j]!=0 && j!=1 && i!=0){
    k<-k+1
    j<-(j-w[i-1])
    i<-which(m[,j] == m[i-1,j])[1]
    elements[k]<-i-1
  }
  
  value<-round(m[n+1,W+1])
  elements<-sort(elements[which(elements>0)])
  
  return(list(value=value,elements=elements))
}


