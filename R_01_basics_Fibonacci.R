###################################################
### BASICS - 05
###################################################

# Write a RECURSIVE function that returns the n value of the Fibonacci sequence.

# Assumptions:
# for n == 0 --> 0
# for n == 1 --> 1
# for n >= 2 --> fib(n) = fib(n-1) + fib(n-2)

fibonacci_1a <- function(n){
  
  if(n == 0 | n == 1){
    return(n)
  }else{
    return(fibonacci_1a(n-1) + fibonacci_1a(n-2))
  }
}

fibonacci_1b <- function(n){
  
  if(n == 0 | n == 1){
    return(n)
  }
  # you can exclude ELSE, works the same:
  return(fibonacci_1b(n-1) + fibonacci_1b(n-2)) 
}


### Fibonacci function, but WITHOUT recursion.

fibonacci_2a <- function(n){
  
  fib = 1
  
  if(n > 1) fib[2] = 1
  
  if(n > 2){
    for(i in 3:n){
      fib[i] = fib[i-1] + fib[i-2]
    }
  }
  return(fib)
}


# More efficient solution:
fibonacci_2b <- function(n){
  # we create string filled with NA values, then we replace them:
  fib = rep(NA, n)
  fib[1] = 1
  
  if(n > 1) fib[2] = 1
  
  if(n > 2){
    for(i in 3:n){
      fib[i] = fib[i-1] + fib[i-2]
    }
  }
  return(fib)
}