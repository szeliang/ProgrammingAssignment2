## This file contains two functions that can be used to store, compute and retrieve a matrix and its inverse.

## Function             makeCacheMatrix
## Description          This function creates a special "matrix" object that can cache its inverse.
## Usage                makeCacheMatrix(x)
## Arguments
#### x                  A matrix to be referenced by the object created. x is assumed to be invertible.
####                    If x is not provided, a 1x1 logical matrix containing NA is initialised.
## Result               A list object containing 4 functions:
#### 1. set(y)          Sets a matrix, y, for this object to reference.
####                    Any inverse matrix previously set is reset to NULL.
#### 2. get()           Retrieves the matrix referenced by this object
#### 3. setInverse()    Sets a matrix to be referenced as the inverse matrix.
####                    To ensure the inverse matrix set is accurate, this function should be called via the cacheSolve function.
#### 4. getInverse()    Retrieves the inverse matrix referenced by this object. 
####                    Returns NULL if inverse matrix has not been set.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function             cacheSolve
## Description          This function computes the inverse of a matrix referenced by an object created using makeCacheMatrix.
##                      If an inverse matrix already exists in the object's cache, then the cached result is returned instead.
## Usage                cacheSolve(x)
## Arguments
#### x                  A list object created using makeCacheMatrix.
## Result               The inverse matrix referenced by the object x.
##                      If a previously set result is obtained from x's getInverse function, then that cached result is returned.
##                      The result (if any) obtained from x's getInverse is always assumed to be correct.
##                      Else, the inverse is computed and set using x's setInverse function. 

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
