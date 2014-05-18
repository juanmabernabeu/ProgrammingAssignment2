## Put comments here that give an overall description of what your
## functions do

## Task - Write a short comment describing this function
## The following method keeps a cache of the matrix and inverse (if calculated)

makeCacheMatrix <- function(x = matrix()) {
  ## Create a variable inverseMatrix to store the inverse
  inverseM <- NA
  ## Set matrix 
  set <- function(y) {
    x <<- y
    inverseM <<- NA
  }
  ## Get matrix
  get <- function() x
  ## Set inverse Matrix
  setInverseMatrix <- function(inverseMatrix) inverseM <<- inverseMatrix
  ## Get inverset Matrix
  getInverseMatrix <- function() inverseM
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Task - Write a short comment describing this function
## The following method calculates the inverse

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseM <- x$getInverseMatrix()
    if(!is.na(inverseM)) {
      message("getting cached data")
      return(inverseM)
    }
    data <- x$get()
    inverseM <- solve(data, ...)
    x$setInverseMatrix(inverseM)
    inverseM
}
