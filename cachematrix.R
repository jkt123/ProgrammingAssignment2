## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## set function if need to modify data for matrix created by makeCacheMatrix
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      ## get/print the matrix currently being used for given special 'makeCacheMatrix'
      get <- function() x
      ## set the inversion of matrix 
      setsolve <- function(solve) inv <<- solve
      ## get/print the inversion of matrix -- this can be null if it's not computed.. 
      getsolve <- function() inv
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get inversion  
  inv <- x$getsolve()
  ## check if the previously solved or not
  if (!(is.null(inv))) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  ## first time solve the matrix inversion
  inv <- solve(mat, ...)
  ## set the solution from first time coomputation
  x$setsolve(inv)
  inv
}
