

parallel_brute_force_knapsack <- function(x, W){
  c <- parallel::detectCores()
  
  n <- length(x$W)
  
  cluster1 <- parallel::makeCluster(c, type = "PSOCK")
  bmatrix <- parallel::parLapply(cluster1, c(1:2^n), function(x){as.integer(intToBits(x)[1:n])})
  
  
  
}
