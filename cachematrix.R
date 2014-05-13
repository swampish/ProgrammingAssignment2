## Functions to perform a caching version of
## matrix inversion (solve()). 

## Create a special "matrix" object that is really
## just a list of functions to get and set the matrix
## and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Calculate the inverse of the matrix set in makeCacheMatrix.
## If the inverse has already been calculated, use the cached version.
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv    
}


