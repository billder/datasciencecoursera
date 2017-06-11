## -------------------------------------------------------------------------------------------
## 
## makeCacheMatrix
## 
## store the cached inverse calculation
## constructor functions to save or return a cached calculation

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## -------------------------------------------------------------------------------------------

## cacheSolve
##
## function to return the inverse of a matrix
## m = placeholder for the inverse
## if m is already set, return it before calculating it again
## if m isn't set, calculate it, then cache it

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
