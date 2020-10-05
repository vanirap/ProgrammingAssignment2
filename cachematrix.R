## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix sets the value of a matrix, gets the value of the matrix, 
# sets value of inverse, gets the value of inverse
## Write a short comment describing this function

makeCacheMatrix <- 
  function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve returns the inverse of the matrix 
#and if inverse is already calculated returns it from the cache 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
