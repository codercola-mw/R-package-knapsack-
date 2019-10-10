#' Using the greedy approach for the knapsack problem
#'
#' @name greedy_knapsack
#'
#' @param x Data frame with two variables v and w
#' @param W knapsack size
#' @return maximum knapsack value and which elements 
#' @export

greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot((W > 0) == TRUE)
  
  dens <- x$v / x$w #define item density, in our case (item value) / (item weight) 
  index <- 1:length(x$v)
  
  #sort items in decreasing order according to density
  x <- x[order(dens, decreasing = T), ]
  
  value <- 0
  weight <- 0
  elements <- c()
  i=1
  while(weight < W){
    weight <- weight + x$w[i] #add weight for every ith object in x
    if(weight < W){
      value <- value + x$v[i] #add value for every ith object in x
      elements <- c(elements, as.numeric(rownames(x)[i]))
    }
  i=i+1
  if(i <= length(x$v)){stop} #stop while loop when all objects have been iterated
}
  return(list(value = round(value), elements = elements))
  
}

