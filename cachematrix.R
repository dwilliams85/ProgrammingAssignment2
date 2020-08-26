## Put comments here that give an overall description of what your
## functions do

## This function will create a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)

}

## This function computes the inverse of the special "matrix x" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
