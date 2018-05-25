## These function take a matrix and compute the inverse but
## only does the computation if the inverse is not cached

## This code sets the initial matrix to NULL if called and sets
## up a list which would hold the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## First this code checks if there is a cached matrix. If there is, it
## returns that matrix. If there is not, it calculates the inverse of
## the given matrix as well as caches the value for future use.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
