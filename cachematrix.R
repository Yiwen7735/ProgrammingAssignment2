## Inverse Matrix Computation and Storage

## Store the inverse matrices that have already been calculated

makeCacheMatrix <- function(x = matrix()) {
  Iv <- NULL
  set <- function(y) {
    x <<- y
    Iv <<- NULL
  }
  get <- function() x
  setiv <- function(inverse) Iv <<- inverse
  getiv <- function() Iv
  list(set = set, get = get,
       setiv = setiv,
       getiv = getiv)
}


## Calculate the inverse matrix of the newly-given matrix

cacheSolve <- function(x, ...) {
  Iv <- x$getiv()
  if(!is.null(Iv)) {
    message("getting cached data")
    return(Iv)
  }
  data <- x$get()
  Iv <- solve(data, ...)
  x$setiv(Iv)
  Iv
}
