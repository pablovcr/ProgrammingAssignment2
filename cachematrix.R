## Defines a special matrix object that allows to cache inverse matrix results
## by using makeCacheMatrix(). To make effective the cache use the cacheSolve funtion

## Create a special matrix that can cache the last calculation for the passed argument

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## If the inverse of a matrix is already cached in the parameter
## it will returned the previously solved matrix inverse, otherwise
## it will calculate it and store it in the cache of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

