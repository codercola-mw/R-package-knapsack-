[![Build Status](https://travis-ci.org/codercola-mw/lab6.svg?branch=master)](https://travis-ci.org/codercola-mw/lab6)
# lab6
 The knapsack package

This package we will create 3 algorithms to the knapsack problem with different computional complexity and how to speedup R code. The goal is to find the knapsack with the largest value of the elements added to the knapsack.

# Set up
```{r, include = TRUE}
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
```

# brute_force_knapsack

```{r, include = TRUE}
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
microbenchmark(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
```


# greedy_knapsack
```{r, include = TRUE}
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
microbenchmark(greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
```


# knapsack_dynamic
```{r, include = TRUE}
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
microbenchmark(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500, fast=FALSE))
```


# Rcpp test
```{r, include = TRUE}
# With Rccp speed
system.time(knapsack_dynamic(x = knapsack_objects[1:100,], W = 3500, fast=TRUE))


# Normal speed
system.time(knapsack_dynamic(x = knapsack_objects[1:100,], W = 3500, fast=FALSE))


```
