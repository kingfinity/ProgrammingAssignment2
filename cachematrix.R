## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its 
        ##inverse.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned 
        ## by makeCacheMatrix above. If the inverse has already been calculated 
        ## (and the matrix has not changed), then cacheSolve should retrieve the
        ## inverse from the cache.
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

## Test ##
## a<-matrix(1:4,2,2)
## n=makeCacheMatrix(a)
## n$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##
## cacheSolve(n)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##
## cacheSolve(n)
##  getting cached data
##      [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5