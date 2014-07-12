## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL

  # Set matrix, set inverse to NULL
  set <- function(y) {
          x <<- y
          inverse <<- NULL
  }

  # Return matrix
  get <- function() x

  # Setter and getter for inverse
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse

  list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Get the inverse from the x object
  inverse <- x$getinverse()

  # If the returned value is not null then we shall
  # return this value.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  # If nothing is returned above, the inverse was null
  # and so we should calculate it.
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)

  # Return the inverse
  i
}
