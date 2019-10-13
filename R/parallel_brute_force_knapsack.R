

parallel_brute_force_knapsack <- function(x, W){
  c <- parallel::detectCores()
  
  n <- length(x$W)
  
  cluster1 <- makeCluster(c, type = "PSOCK")
  bm <- parLapply(cluster1, c(1:2**n), function(x){as.integer(intToBits(x)[1:n])})
  stopCluster(cluster1)
  cluster2 <- makeCluster(c, type = "PSOCK")
  w1 <- simplify2array(parLapply(cluster2, bm, function(y){y%*%x$W}))
  stop(cluster2)
  cluster3 <- makeCluster(c, type = "PSOCK")
  v1 <- simplify2array(parLapply(cluster3, bm, function(y){y%*%x$v}))
  stopCluster(cluster3)
  
  v1[w1 > W]=0
  maxValIdx <- which.max(v1)
  
  elements_Idx <- bm[[maxValIdx]]
  
  elements <- c(c(1:n) * elements_Idx)
  elements <- elements[elements > 0]
  
  return(list(value = round(maxValIdx), elements = elements))
  
  
  
}
