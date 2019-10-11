parallel_brute_force_knapsack <- function(x, W){
  c <- parralel::detectCores()
  
  n <- length(x$W)
  
  cluster1 <- makeCluster(c, type = "PSOCK")
  bmatrix <- parLapply(cluster1, c(1:2^n), function(x){as.integer(intToBits(x)[1:n])})
  
  
  
}