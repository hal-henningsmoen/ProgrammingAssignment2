## This file contains functions that implement caching of matrix inversion 
## How to use:
## 1) Create a matrix 
## 2) Pass the matrix to makeCacheMatrix to create a cacheMatrix
## 3) Pass the cacheMatrix to cacheSolve.   
##    the cacheSolve function behaves the same as way as solve() except that it caches the result of the inversion in the 
##    cacheMatrix. This means it will not have to recalcualte the inverse more than once.  This also means it will fail if the
##    matrix is not invertible.
##   Example usage
##   x <- matrix(1:4, 2, 2 )
##   m <- makeCacheMatrix( m )
##   cacheSolve( m )
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  RUNNING THE FUNCTION AGAIN ON m
##  cacheSolve( m )
##  getting cached data
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5 

## This function creates and returns cacheMatrix
## It accepts a matrix as input

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The first time this function is called on a cacheMatrix it calculates the inverse of the matrix, caches and returns the result.
## On subsequent calls it just returns the cached result
## The function takes a cacheMatrix as its first parameter.  The extra parameters are passed forward to the solve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

