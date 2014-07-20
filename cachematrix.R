## These are functions that caches the inverse of a matrix.

## This function creates a matrix object with an "inverse" "attribute" (intitially set to NULL).
## The inverse attribute it used to cache the inverse of the matrix since inverting a matrix
## is a time consuming task.
##
## This object is really a list of functions to set the matrix, set() , return the matrix, get() and
## to set and get the inverse of the matrix, setinverse() and getinverse().

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


## The cacheSolve function returns the inverse of  a matrix. In case the
## inverse is already cached in the "inverse" "attribute" then
## cached inverse is returned otherwise the inverse is calculated.

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

  # Set the inverse in the x object.
  x$setinverse(i)

  # Return the inverse
  i
}
