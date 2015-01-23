## The functions allow for retrieving cached values of calculations.

## makeCacheMatrix() creates a data and result pair construct.
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve() looks up the result, if it's not available, it calculates
## and stores the result; otherwise it just returns the result.
cacheSolve <- function(cm, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- cm$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- cm$get()
    inv <- solve(data, ...)
    cm$setinverse(inv)
    inv
}
