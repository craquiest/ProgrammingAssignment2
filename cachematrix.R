## makeCacheMatrix & cacheSolve in this file are functions meant to
## save users time and computer ressources when dealing with computing
## the inverse of a matrix

## makeCacheMatrix creates a matrix "object" 
## able to stash its own inverse for future use

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve returns the inverse of a "matrix object" created with makeCacheMatrix
## it compute s inverse the 1st time, and retrieve cache any subsequent times
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("reaching for the cache")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
