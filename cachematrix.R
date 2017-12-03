## These functions cache a matrix, makeCacheMatrix, and allow for returning
## the inverse, makeCacheMatrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL

    set <- function(y) {
        if (all(x == y)) {
            message("All values are the same, leaving inverse alone")
        }
        else {
            message("All values are not the same, resetting inverse")
            x <<- y
            inverseMatrix <<- NULL
        }
    }
    get <- function() x
    setInverse <- function(solve) inverseMatrix <<- solve
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Retrieve the matrix inverse from the cache.
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
