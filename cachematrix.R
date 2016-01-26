## makeCacheMatrix creates matrix with cacheable inverse
## cacheSolve calculates the inverse for the matrix created 
## with makeCacheMatrix.

## Creates a matrix which is capable of caching the inverse
## matrix. One should use set and get of the returned object
## to update and to retrieve the matrix. Matrix can be given
## through parameter x. If none is given then an empty
## matrix is created.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inv <<- inv
    getInverse <- function() inv
        list(set = set, get = get,
             getInverse = getInverse,
             setInverse = setInverse)
}


## Calculates the inverse of the given matrix created with
## makeCacheMatrix. In case the inverse has already been calculated
## then the cached inverse is returned. Matrix is given
## in parameter x and the parameters which go to the solve
## are given after that. Use ?solve for description of
## these parameters.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
		message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
