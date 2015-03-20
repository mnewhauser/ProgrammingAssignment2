##This function allows you to set and get the value of a matrix and set and get the value of the 
##inverse of the matrix.

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


##This function calculates the inverse of the matrix created with the previous function
##. It checks to see if the inverse has already been calculated and if so, it retrieves 
## the value from the cache instead of doing a whole new computation. If not, the function 
## calculates the inverse of the matrix and sets the value of the inverse in the cache via the
##setinverse function.

cacheinverse <- function(x, ...) {
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
