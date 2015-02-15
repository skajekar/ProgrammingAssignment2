## This code saves the precomputed inverse value in cache
## 

## Functions related to cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    
    list(set = set, get = get,
         setMatrix = setMatrix, getMatrix = getMatrix)
}


## Solve the matrix inverse with cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("Getting data from cache")
    return(m)
  }
  ## When not found in cache, compute it
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
