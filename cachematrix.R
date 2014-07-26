## `makeCacheMatrix` and `cacheSolve` functions can be used in pair
## to cache an inverse for a given matrix (assumuming the matrix is invertible)


## `makeCacheMatrix` function creates a list containing functions
## to get and set values of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(y) x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## `cacheSolve` function calculates or returns a previosely cached inverse 
## for the `x` matrix created with the `makeCacheMatrix` function.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
