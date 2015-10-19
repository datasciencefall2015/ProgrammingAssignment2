## Matrix inversion is usually a costly computation.
## This module contains pair of functions that cache the inverse of a matrix.

## Returns a storage for matrix and it's corresponding inverse form

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    getMatrix <- function() x
    getInverse <- function() inverse

    setMatrix <- function(value) {
        x <<- value
        inverse <<- NULL
    }
    setInverse <- function(value) {
        inverse <<- value
    }

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Returns inverse matrix, all costly computations are executed only once

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(is.null(inverse)) {
        message("calculating inverse matrix")
        inverse <- solve(x$getMatrix(), ...)
        x$setInverse(inverse)
    }
    inverse
}
