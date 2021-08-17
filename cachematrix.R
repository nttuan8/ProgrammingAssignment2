## Put comments here that give an overall description of what your
## functions do
## store the cache of inverse of matrix

## Write a short comment describing this function
## Input a matrix then output a list with four functions
## get, set, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Check the inverse cache, if exist then return the inverse
## Otherwise, calculate the inverse and store cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if (!is.null(i)){
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
