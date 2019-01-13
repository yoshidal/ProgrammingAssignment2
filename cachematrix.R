## This file contains 2 funcions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function
## 
## Description: creates a list containing 4 functions:
## 1. set: sets the value of the matrix
## 2. get: gets the matrix
## 3. setInverse: sets the inverse of the matrix
## 4. getInverse: gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(informedXInverse) xInv <<- informedXInverse
    
    getInverse <- function() xInv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function
## 
## Description: calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the 
## inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix  
## and sets the value of the inverse matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    xInv <- x$getInverse()
    if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInverse(xInv)
    xInv
}
