## We have 2 functions here: 
## 1. makeCacheMatrix() - This function creates a special "matrix" object that can cache its inverse.
##      Output - A list containing the functions to 
##               a. Set the Matrix
##               a. Get the Matrix
##               a. Set the Inverse
##               a. Get the Inverse
##
makeCacheMatrix <- function(functionlist = matrix()) {
        CachedInverse <- NULL
## To assign value of matrix and mean in a different environment.
        setmatrix <- function(mat) {                    
                functionlist <<- mat
                CachedInverse <<- NULL
        }
        getmatrix <- function() {
                functionlist
        }
## To cache the value of inverse for a matrix.
        setinverse <- function(solve) {
                CachedInverse <<- solve
        }
## To retrieve the cached value of Inverse of matrix.
        getinverse <- function() {
                CachedInverse
        }
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}
##
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
##                already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the 
##                cache.
##      Input  - List created by makeCacheMatrix() function.
##      Output - Returns the Inverse of the matrix by either getting it from cache or calculating it if not present in cache.
##
cacheSolve <- function(functionlist, ...) {
        CachedInverse <- functionlist$getinverse()
        if (!is.null(CachedInverse)) {
                message("Getting inverse from cached data")
                return(CachedInverse)
        }
        inpmat <- functionlist$getmatrix()
        CachedInverse <- solve(inpmat, ...)
        functionlist$setinverse(CachedInverse)
        return(CachedInverse)
}
