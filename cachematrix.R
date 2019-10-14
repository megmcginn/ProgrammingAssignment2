## makeCacheMatrix returns 4 functiond, in a list
## The first function sets the value of the matrix 
## The 2nd  gets the matrix
## The 3rd function sets the value of the inverse of the original matrix
## The 4th function gets the value of the inverse of the original matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calcuates the inverse of the original matrix, usiing the solve function
## Before calculating the inverse, it checks to see if the value of the inverse was already saved.
# It uses makeCacheMatrix to check this

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
