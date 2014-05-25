# This function creates a special "matrix" object that can cache its inverse.
# Example usage:
# > x <- matrix(rnorm(9), nrow = 3)           // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, return cached inverse



makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL

    # Set for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get matrix
    get <- function() x

    # Set inverse
    setinv <- function(inverse) inv <<- inverse
    # Get inverse
    getinv <- function() inv

    # Return the matrix with newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}





## Below function returns a matrix that is the inverse of 'x'
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}


