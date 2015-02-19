## These function enable a matrix and its inverse to be cached so the inverse does not
## need to be recomputed each time it is needed. Example run:
##
## > x <- makeCacheMatrix(matrix(c(2, 4, 3, 1), nrow=2, ncol=2))
## > x$get()
##      [,1] [,2]
## [1,]    2    3
## [2,]    4    1
## > x$getinverse()
## NULL ## not yet cached because cacheSolve has not run
## > cacheSolve(x)
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2
## > cacheSolve(x)
## getting cached data  ## second call to cacheSolve gets previously cached result
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2
## > x$getinverse() ## cached value available because cacheSolve was run
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2


## Function returns a list of functions that manage a cached matrix object. The list includes:
## set: sets the value of the matrix (e.g. updates the matrix after its created)
## get: gets the value of the matrix
## setinverse: stores the inverse matrix (computed by cacheSolve) in the cache
## getinverse: gets the inverse of the matrix (null if not yet computed and cached, via setinverse, by cacheSolve)

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


## Function to solve (invert) the matrix x and cache the inverted matrix via these steps:
## get the inverse matrix from the cache
## if cache was full, print message and return cached inverse matrix
## if cache was empty, get the cached original matrix, compute its inverse, store the inverse matrix in the cache and return the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
