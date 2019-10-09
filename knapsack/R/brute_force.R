set.seed(42)
n <- 10
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


brute_force_knapsack <- function(x, w){
  if(x = 0 | W = 0){
    return(0)
  }
  
  i=2 #start row counter so it starts in the first value of x.
  optimum_value = 0     
  selected_items = c()
  weights <- c()
  values <- c()
  while(i<=nrow(x)) #keep going through rows until we reach the end.
  {
    w <- as.data.frame(combn(x[,1], i)) #makes all combinations of the weights.
    v <- as.data.frame(combn(x[,2], i)) #makes all combinations of the values.
    sumw <- colSums(w) #find which is largest.
    sumv <- colSums(v)
    weights <- which(sumw<=W) #choose the sum which is largest from all weight combinations.
    if(length(weights) != 0){ 
      values <- sumv[weights]
      optimum_value <- max(values)
      temp<-which((values) == optimum_value)
      maxValWghtIdx <- weights[temp]
      maxValWght <- w[, maxValWghtIdx]
      j <- 1
      while (j<=i){
        selected_items[j]<-which(X[,1]==maxValWght[j])
        j=j+1
      }
    }
    i=i+1
    
  }
  
  return(list(value=round(optimum_value),elements=selected_items))
}