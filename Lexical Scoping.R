## ----------------------------------------------------------------------------##
## makeCacheMatrix creates a list containing a function for the following:
# -> sets the value of the matrix
# -> gets the value of the matrix
# -> sets the value of inverse of the matrix
# -> gets the value of inverse of the matrix
##---------------------------------------------------------------------------------------------------------##

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
##---------------------------------------------------------------------------------------------------------##
## CacheSolve() returns the inverse of the matrix.
## It first checks if the inverse has already been computed. If yes, then the function gets the result and does not perform any computation.
## However, if the inverse hasn't been calculated, it will computes the inverse and then set the value in the cache via setInverse function.
## Also this function assumes that the inverse of the matrix exists.
##---------------------------------------------------------------------------------------------------------##

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}
