## Put comments here that give an overall description of what your
## functions do

## This function creates a cached version of a matrix

makeCacheMatrix <- function(x = matrix()) {

        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) c <<- inverse
        getInverse <- function() c
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function returns the inverse of a matrix from cache, if one exists, otherwise it returns a calculated invese matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        c <- x$getInverse()
        if(!is.null(c)) {
                message("getting cached inverse matrix data")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$setInverse(c)
        c
        
        
}
