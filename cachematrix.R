## These methods create a CacheMatrix to hold the inverse of a matrix in cache.
## If you have a square invertible matrix M, first you create a CacheMatrix
## using makeCacheMatrix:
## CM <- makeCacheMatrix(M)
## then you find its inverse
## cacheSolve(CM)
## Later if you need CM's inverse you can call
## cacheSolve(CM)
## and it will return CM's cached inverse or recalculate the inverse
## if CM has changed.
## Methods for a CacheMatrix CM:
## CM$set(Matrix), will set a new matrix for CM to hold
## CM$get(), returns the matrix that CM holds
## CM$setInverse(inverseMatrix), sets the inverse of CM, should be set by a call
##                               to cacheSolve(CM), and not set by users.
## CM$getInverse(), returns the inverse of CM, users should use cacheSolve(CM),
##                  to check for a cached inverse.

##Creates a CacheMatrix to hold the inverse of the matrix
## M - the Matrix to be turned into a CacheMatrix
makeCacheMatrix <- function(M = numeric()) {
        inverse <- NULL
        set <- function(Y) {
                M <<- Y
                inverse <<- NULL
        }
        get <- function() M
        setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Gets the inverse of a CacheMatrix, uses solve() to find it if it is NULL
## cacheMatrix - the CacheMatrix, whose inverse is returned.
cacheSolve <- function(cacheMatrix, ...) {
        inverse <- cacheMatrix$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        M <- cacheMatrix$get()
        inverse <- solve(M, ...)
        cacheMatrix$setInverse(inverse)
        inverse
}