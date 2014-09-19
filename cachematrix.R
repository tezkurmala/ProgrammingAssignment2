## makeCacheMatrix and cacheSolve functions work together to save time and 
## give great performance by caching the inverse of a matrix.
## Following is a simple example on how these function can be used together
## mySimpleMatrix <- matrix(c(1,1,0,1),2,2)
## cacheMatrixObj <- makeCacheMatrix(mySimpleMatrix)
## cacheSolve(cacheMatrixObj) - Computes and Returns Inverse of the because this is the first time
## cacheSolve(cacheMatrixObj) - Returns the Inverse of the matrix that is already computed and cached

## makeCacheMatrix takes a square invertible matrix as argument. 
## It provides an environment to cache the Inverse of the matrix. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(invMatrix) m <<- invMatrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes an object(x) created by makeCacheMatrix fuunction as the first argument
## cacheSolve verifies if the first argument already has inverse of the matrix. 
## If one exists, the same will be returned. 
## Else, inverse of matrix is computed, cached (in x) and then returned

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Cached data found")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
