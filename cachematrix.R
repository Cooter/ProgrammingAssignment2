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

##Creates a CacheMatrix to hold the inverse of the matrix
## M - the Matrix to be turned into a CacheMatrix
makeCacheMatrix <- function(M = numeric()) {
        inverse <- NULL    # Inverse of matrix M, set to null
        # Create functions for dealing with a CacheMatrix
        # Method to set the matrix in a CacheMatrix, set unknown inverse to NULL
        set <- function(Y) {
                M <<- Y
                inverse <<- NULL
        }
        # Method to get the matrix in a CacheMatrix
        get <- function() M
        # Method to set the inverse a CacheMatrix
        # Note: this inverse is set by a call to cacheSolve
        setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
        # Method to get the inverse of a CacheMatrix. Usually, the method
        # Note: cacheSolve should be used to get the inverse.
        getInverse <- function() inverse
        # Return a list of the functions, as the interface to a CacheMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Gets the inverse of a CacheMatrix, uses solve() to find it if it is NULL
## cacheMatrix - the CacheMatrix, whose inverse is returned.
cacheSolve <- function(cacheMatrix, ...) {
        # See if the CacheMatrix already has an inverse.
        # If so get the Cached inverse.
        inverse <- cacheMatrix$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        # Otherwise, get the Matrix in the CacheMatrix
        M <- cacheMatrix$get()
        # Find its inverse
        inverse <- solve(M, ...)
        # Cache and return the inverse
        cacheMatrix$setInverse(inverse)
        inverse
}