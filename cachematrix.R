## First function "makeCacheMatrix" creating a list with 4 functions included and caches data.
## Second function "cacheSolve" calculates inverse matrix via solve() function and writes it to the cache of list provided in parameters

## Function provide user with 4 functions in a list which help the user to set and get value of the matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function (y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invMatrix) im <<- invMatrix
  getInvMatrix <- function() im
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Function checks whether cached value of inverse function is available if yes - provides it, if no - calculates new.

cacheSolve <- function(x, ...) {
  im <- x$getInvMatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInvMatrix(im)
  im
}
