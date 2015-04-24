## This files contains functions that help to solve and store in the cache
## the inverse of a matrix

## makeCacheMatrix just creates a function that contains 4 functions that help
## us to create a sort of object that can store one or more values in the cache

makeCacheMatrix <- function(x = matrix()) {
        #makes a special matrix
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


## cacheSolve computes the inverse of a our special matrix (if not stored in
## the cache), and store the value in the special matrix's caché. If the inverse
## was already stored, just retrieve that value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
