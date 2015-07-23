## makeCacheMatrix and cacheSolve are two functions that allow 
## for the caching the calculation of the inverse of a matrix. 
## The functions assume that the matrix is always invertible. 

## makeCacheMatrix function creates a special matrix object that 
## can cache its inverse. It allows for getting and setting the
## matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated the function, it will be
## retrieved from the cache. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
