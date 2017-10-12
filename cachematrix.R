## The following two functions calaculate, cache and retreive the inverse of a matrix.
## Example:
## X = matrix(c(2,4,3,1,1,1,1,1,2),3,3) # a 3x3 matrix
## CM <- makeCacheMatrix()
## CM$setMatrix(X)
## cacheSolve(CM)

## The following function creates a special "matrix" 
## object that sets and gets the value of the input 
## matrix X and the value of its inverse, INV.

makeCacheMatrix <- function(X = matrix()) {
  INV <- NULL
  setMatrix <- function(Y) {
   X <<- Y
   INV <<- NULL
  }
  
  getMatrix <- function() {
    X
  }
  
  setInverse <- function(matrixInverse) {
    INV <<- matrixInverse
  }
  
  getInverse <- function() {
    INV
  }
  
  list(getMatrix = getMatrix, setMatrix = setMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## The following function recieves as input the special
## matrix object created by makeCacheMatrix and calculates
## its inverse, if it was not previously calculated and
## stored in the function cache.

cacheSolve <- function(X, ...) {
  INV <- X$getInverse()
  if (!is.null(INV)) {
    message("Getting cached data.")
    return(INV)
  }
  MAT <- X$getMatrix()
  INV <- solve(MAT, ...)
  X$setInverse(INV)
  INV
}
