## This file has 2 functions, one ( makeCacheMatrix ) to hold a cached copy of the inverse of a matrix
## The other function ( cacheSolve ) either calculates the inverse of a matrix and stores it in the cache
## or if the inverse is already calculated, it just retrieves it instead of calculating it again

## Usage
## d <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
## s <- makeCacheMatrix()
## s$set(d)
## The next call computes and stores the inverse
## cacheSolve(s)
## Calling cacheSolve(s) again should retrieve it from the cache
## cacheSolve(s)
## Another matrix to test
## mat <- matrix(c(1,1,1,3,4,3,3,3,4), nrow = 3, ncol = 3)

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set<-function(y){
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    
    getinverse <- function() inverse
    
    list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if( !is.null(inv) ){
        message("getting cached inverse matrix data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data)
    
    x$setinverse(inv)
    
    inv
}
