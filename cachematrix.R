## This program utilizes 2 functions that cache the inverse of a matrix.

## Below  function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {  # set the value of the matrix
    x <<- y
    i <<- NULL
  }
  
  get <- function() x  # get the value of the matrix
  setI <- function(inverse) i <<- inverse  # set the value of the inverse of the matrix
  getI <- function() i # get the value of the inverse of the matrix
  list (
    set = set,
    get = get,
    setI = setI,
    getI = getI
  )

}


## cacheSolve  function computes the inverse of the makeCacheMatrix function. 
# it first checks to see if the inverse has already been calculated. If so, it get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix  

cacheSolve <- function(x, ...) {
  i <- x$getI()
  if(!is.null(i)) {  # check if the inverse of matrix is already cached
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) # compute the inverse of matrix
  x$setI(i)
  i
}
