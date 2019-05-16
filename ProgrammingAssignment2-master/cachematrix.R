## Put comments here that give an overall description of what your functions do
## The first function, makeVector, creates a special "vector", which is really a 
## list containing a function to set the value of the vector,get the value of the vector,
## set the value of the mean, and get the value of the mean.

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Solution 1.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The following function calculates the mean of the special "vector" created with the above function. However,
## it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


## Solution 2. Here I find the function of the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv 
}


## end