#' `makeCacheMatrix` function creates a list containing functions to get
#' and set values of the matrix and its inverse
#'
#' @param matrix
#' @return list containing functions to get and set values of the matrix and its inverse
#' @export
#' @examples
#' makeCacheMatrix(m)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    ## making sure that we erase previousely cached inverse
    inverse <<- NULL
  }
  get <- function(y) x
  ## `<<-` operator enables to "cache" inverse matrix once
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' `cacheSolve` function calculates or returns a previosely cached inverse 
#' for the `x` matrix created with the `makeCacheMatrix` function.
#' 
#' @param list of functions to get and set values of the matrix and its inverse
#' @return inverse matrix (either cached or just computated)
#' @seealso makeCacheMatrix
#' @export
#' @examples
#' cacheSolve(makeCacheMatrix(m))
cacheSolve <- function(x, ...) {
  ## first we check if there is a cached inverse for a given x
  inverse <- x$getinverse()
  ## if there is one we return it right away 
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## otherwise we get the actual matrix
  data <- x$get()
  ## computate an inverse (passing "extra" parameters to `solve` function)
  inverse <- solve(data, ...)
  ## and cache it
  x$setinverse(inverse)
  inverse
}

