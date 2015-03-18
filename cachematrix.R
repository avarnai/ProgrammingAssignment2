## These functions calculate and cache the inverse of a matrix.


## function makeCacheMatrix
##
## This function creates a special "matrix" object that can cache its 
##   inverse. The "matrix" object is really a list containing a function to
##    - set the value of the input matrix,
##    - get the value of the input matrix,
##    - set the value of the inverse matrix,
##    - get the value of the inverse matrix.

makeCacheMatrix <- function(mat = matrix()) {
    matinv <- NULL  # define new variable with 'no value' assigned
    
    set <- function(matnew) {  # change to new matrix and clear the cached vaue of the inverse matrix
        mat <<- matnew
        matinv <<- NULL
    }
    get <- function() mat  # returns matrix
    
    setinverse <- function(inv) matinv <<- inv  # sets the cached value of the inverse matrix 
    getinverse <- function() matinv  # returns the cached value of the inverse matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve
##
## This function computes and returns the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been
##   calculated (and the matrix has not changed), then cacheSolve retrieves
##   the inverse from the cache.

cacheSolve <- function(cachemat, ...) {
    inv <- cachemat$getinverse()  # gets the cached inverse matrix
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)  # returns cached inverse matrix if not 'NULL'
    }
    data <- cachemat$get()  # the matrix
    inv <- solve(data, ...)  # computes the inverse matrix
    cachemat$setinverse(inv)  # caches the inverse matrix (calls function in the function...)
    inv  # 
}