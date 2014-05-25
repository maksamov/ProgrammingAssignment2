## This function takes a matrix and provides methods for:
##   1. setting a new and getting the current matrix
##   2. setting and getting the inverse of the matrix
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## This functions gets the inverse of the matrix. 
## If matrix inverse already exists in the cache it returns the cached inverse.
## If the inverse does not exist, it computes it using the solve() function and sets
## the inverse for the matrix, effectively caching it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
