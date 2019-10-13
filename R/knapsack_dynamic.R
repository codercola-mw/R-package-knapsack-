#' Using the dynamic approach for the knapsack problem
#'
#' @name knapsack_dynamic
#'
#' @param x Data frame with two variables v and w
#' @param W knapsack size
#' @param fast to choose to use Rcpp or not
#' @return maximum knapsack value and which elements 
#' @export
#' @importFrom Rcpp cppFunction
knapsack_dynamic <- function(x, W, fast=FALSE){
  stopifnot(is.data.frame(x))
  stopifnot((W > 0) == TRUE)
  stopifnot(colnames(x) == c("w","v"))
 if(fast ==  FALSE){
  w<- x$w
  v <- x$v
  m <- matrix(ncol = W+1, nrow=nrow(x)+1)
  m[1,] <- rep(0, W+1)

  for(i in 1:nrow(x)){
    for(j in 0:W){
      if (w[i] > j){
        m[i+1, j+1] <- m[i, j+1]
      }
      else{ m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-w[i]]+v[i])}
    }
  }
  
  #looking from the back elements
  j <- j+1
  i <- which.max(m[,j])
  elements<-length(nrow(x))
  k <- 1
  elements[k] <- i-1
  
  while(m[i,j]!=0 && j!=1 && i!=0){
    k<-k+1
    j<-(j-w[i-1])
    i<-which(m[,j] == m[i-1,j])[1]
    elements[k]<-i-1
  }
  
  value<-round(m[nrow(x)+1,W+1])
  elements<-sort(elements[which(elements>0)])
  
  return(list(value=value,elements=elements))
  
 } 
  else{
      
    Rcpp::cppFunction('NumericVector knaprcpp(NumericVector v, NumericVector w, int W){
    
    int n = w.size();
    NumericMatrix val_mat(n+1, W);
    NumericMatrix mat(n+1, W);
    double val1;
    double val2;
    int w_l;
    int index = 0;
    NumericVector result(n+1);
    int W1 = W-1;
    int object = n-1;
    
       for(int i = 1; i<n+1; i++){

        val1 = 0;
        val2 = 0;
        
        for(int j =0; j <W; j++){
         val1 = val_mat(i-1, j);
         w_l = j - w(i-1);
              
          if(w_l>=0){
          val2 = v(i-1) + val_mat(i-1, w_l+1);
          }

          if(val2>val1){
          mat(i,j)= 1;
          val_mat(i,j)=val2;
          }
              else{
              mat(i,j)= 0;
              val_mat(i,j)=val1; 
              }

    }}
    result(index) = val_mat(n-1, W-1);
   
    while(object>0 && W1>0){
    if (mat (object, W1) == 1){
      index = index +1 ;
      result(index) = object;
      object = object -1;
      W1 = W1 - w(object)+1;
    }
    else{object = object -1 ;}
    }
    
    
    return result;
  }
              ')
      
      x <- x[order(x[,1]), ]
      Rcpp_vec <- knaprcpp(x[,2], x[,1], W)
      
      a<-list()
      
      a[["value"]]<-round(Rcpp_vec[1])
      Rcpp_vec<-Rcpp_vec[which(Rcpp_vec>0)]
      Rcpp_vec<-as.numeric(rownames(x)[Rcpp_vec])
      a[["elements"]]<-Rcpp_vec[-1]
      
      return(a)
    }
    
}
  
#knapsack_dynamic(x=knapsack_objects[1:80,], W=5000, fast=TRUE)

