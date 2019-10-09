#' Using brute force search for the knapsack problem
#'
#' @name brute_force_knapsack
#'
#' @param x Data frame with two variables v and w
#' @param W knapsack size
#' @return maximum knapsack value and which elements 
#' @export
#' @importFrom utils combn


brute_force_knapsack <- function(x, W){
  stopifnot(x > 0, W > 0)
  
  #start row counter so it starts in the first value of x, and start empty lists.
  i=2 
  most_valuable = 0     
  choose_item <- c()
  weight_list <- c()
  value_list <- c()
  
  #keep going through rows until we reach the end.
  while(i <= nrow(x)){ 
    
    w1 <- as.data.frame(combn(x[,1], i)) #make all combinations of the weights.
    v <- as.data.frame(combn(x[,2], i)) #make all combinations of the values.
    sum_w <- colSums(w1)
    sum_v <- colSums(v)
    weight_list <- which(sum_w <= W) #choose the sum which is largest from all weight combinations.
    
    #find item with largest value for every entire weight_list.
    if(length(weight_list) != 0){ 
      value_list <- sum_v[weight_list]
      most_valuable <- max(value_list)
      t <- which((value_list) == most_valuable)
      maxValWghtIdx <- weight_list[t]
      maxValWght <- w1[,maxValWghtIdx]
      
      #add number of chosen items with max value to choose_item.
      j <- 1
      while (j<=i){
        choose_item[j] <- which(x[,1] == maxValWght[j])
        j=j+1
      }
    }
    i=i+1
    
  }
  
  return(list(value = round(most_valuable), elements = choose_item))
}
