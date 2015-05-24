## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinversematrix <- function(invmatrix) inverse <<- invmatrix
  getinversematrix <- function() inverse
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinversematrix()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, diag(nrow(data)) ...)
  x$setinversematrix(inverse)
  inverse
}
