## this solution contains two functions (makeCacheMatrix and cacheSolve) that compute and cache the inverse of a matrix 
## for faster lookup 

## function makeCacheMatrix
## creates an Object that can cache a matrix an its inverse
## There are 4 methods that can be called on the object: 
## set: sets the matrix and reset the inverse to NULL
## get: gets the matrix
## setInverseMatrix: sets the inverse matrix for the object
## getInverseMatrix: gets the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  ## define variable for the inverse matrix, initialize to NULL
  inv <- NULL
  ## sets the copy of the matrix and NULL for the inverse matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## gets the copy of the matrix
  get <- function() x
  ## sets the copy of inverse matrix
  setInverseMatrix <- function(i) inv <<- i
  ## gets copy of the inverse matrix
  getInverseMatrix <- function() inv
  ## returns a list with the methods that can be called for dealing with the matrix and inverse matrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## function cacheSolve
## return a matrix that is the inverse of the one passed into it 
## if the inverse was already computed for that matrix then the result is returned from cache

cacheSolve <- function(x, ...) {
  ## get inverse matrix from cache if available
  inv <- x$getInverseMatrix()
  ## if the inverse was found in cache, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise, compute and cache inverse matrix since it might be used later
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverseMatrix(inv)
  ## return inverse matrix 
  inv
}