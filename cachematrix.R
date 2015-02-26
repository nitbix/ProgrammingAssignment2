## Two functions to auto-cache (memoize) the inverse of a given matrix.
## If the inverse is being fetched for the first time, calculate it.
## Otherwise, retrieve it from cache.

## This will create a special "cache matrix" that will cache its inverse
## once it is calculated.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This will return a matrix that is the inverse of 'x', and only calculate it
## once.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setinverse(i)
    i
}
