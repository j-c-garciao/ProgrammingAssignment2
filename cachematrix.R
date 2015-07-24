## Developed by JC García-Ojeda (2015)

## The following functions do (i) create a special matrix object than cacahe 
## its inverse and (ii) compute its inverse.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse 

## Example
## x<-makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 9, 0, 4),nrow=3,ncol=3))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function cacheSolve  the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Example
## cacheSolve(x)

## Solution
## [,1]        [,2]        [,3]
## [1,]  0.14184397  0.41843972 -0.31914894
## [2,] -0.11347518 -0.13475177  0.25531915
## [3,]  0.09219858 -0.07801418  0.04255319


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## computing the inverse of matrix "data"
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
