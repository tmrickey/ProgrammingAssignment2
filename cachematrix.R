## Project 2 - Caching the inverse of a matrix


## This function creates setters and getters for the matrix and
## the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
}
    get <- function() x
    setInverseMatrix <- function(solved) inverseMatrix <<- solved
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function takes a list created by makeCacheMatrix() and
## either returns the cached inverse matrix or calculates the 
## inverse matrix if it is not cached.

cacheSolve <- function(x, ...) {
## Checks if the inverse matrix is cached and returns it
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
## Calculates the inverse matrix if it was not cached
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
