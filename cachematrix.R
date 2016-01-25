## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special matrix that can 
## cache its inverse. It provides a list of functions to get/set
## the value of the matrix as well as get/set the value of the 
## matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special
## (invertible) matrix returned by makeCacheMatrix. 
## If the inverse  of the matrix has already been calculated and 
## cached, and it is not changed, the inverse is retrieved from 
## the cache, otherwise it is calculated, cached and returned. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
