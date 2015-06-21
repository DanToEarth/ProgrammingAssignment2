makeCacheMatrix <- function(x = matrix()) {
  # set a null value
  inv <- NULL
  # define the set function
  set <- function(y) {
    # use '<<-' for assignment from parent level
    x <<- y
    inv <<- NULL
  }
  # get x
  get <- function() x
  # define function to set inverse
  setinverse <- function(inverse) inv <<- inverse
  # define function to get inverse
  getinverse <- function() inv
  # return a named list of se, get, setinverse and getinverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  # assign getinverse to inv
  inv <- x$getinverse()
  # if inv is null then notify to get the cached data
  if(!is.null(inv)) {
    message("getting the cached data.")
    return(inv)
  }
  # get the matrix data
  data <- x$get()
  # solve the inverse
  inv <- solve(data)
  # return the inverse
  x$setinverse(inv)
  # return the inverse
  inv
}
