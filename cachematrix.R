## this code is for Caching the Inverse of a Matrix:
## The Matrix inversion is usually a costly computation and there may be some 
## The benefit is to caching the inverse of a matrix rather than compute it repeatedly.
## following are a pair of functions that are used to create a special object that 
## sthis tores a matrix and caches its inverse.
## And this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Here is a function to computes the inverse of the special "matrix" 
## the makeCacheMatrix above, In case if the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
