## Coursera - R-Programming - Assignment 2 
## by Erik Pinter
##
## Two functions to cache a matrix and cache the inverse of that matrix in memory


## makeCacheMatrix():   creates a special "matrix" that can store its reverse
## cacheSolve():        computes the inverse of the special "matrix" returned by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     ## clear cached inverse
        
        set <- function(y) {            ## function set: cache new matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x             ## get the cached matrix
        setinverse <- function(solve) inv <<- solve     ## set the inverse of the matrix
        getinverse <- function() inv                    ## get the cached inverse
        list(set = set, get = get,x                     ## create list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve():        computes the inverse of the special "matrix" returned by makeCacheMatrix
##                      If the inverse has already been calculated (and the matrix has not changed), then the 
##                      cachesolve will retrieve the inverse from the cache  

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()           ## get the inverse matrix from memory
        
        if(!is.null(inv)) {             ## if the inverse matrix exists, return the cached data
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                 ## get the cached matrix 
        inv <- solve(data, ...)         ## calculate inverse matrix
        x$setinverse(inv)               ## cache the inverse matrix in memory
        inv
}

