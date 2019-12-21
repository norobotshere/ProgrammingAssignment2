## Provides two functions, makeCacheMatrix() and cacheSolve().
## makeCacheMatrix() creates a special object that wraps a normal
## matrix and provides a place to store a cached inverse.
## cacheSolve() will use the special object created by makeCacheMatrix()
## to solve its inverse, caching it in the process. If it has already
## been computed, the cached value is used.

## Returns a wrapper for a normal matrix that can store its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x

    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Uses a matrix wrapper created by makeCacheMatrix() to compute its
## inverse. Uses a cached value if already computed.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("using cached inverse")
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)

    i
}
