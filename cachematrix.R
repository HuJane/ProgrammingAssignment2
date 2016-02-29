## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  get <- function() x ## X is a square invertible matrix
  setinverse <- function(solve) m <<- solve ## the solve function is computing the inverse of a square matrix
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)){ 
    message("getting cached data")
    return(m)
  }
  y<-x$get()
  m<-solve(y,...)
  x$setinverse(m)
  m
}
