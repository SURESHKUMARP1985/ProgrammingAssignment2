# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
# Here two functions makeCacheMatrix, cacheSolve  are used to compute the inverse of matrix.
# The program assumes the matrix is always invertible.


# The function makeCacheMatrix  creates a matrix, which is a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# The cacheSolve function calculates the inverse of the special matrix created with the above function. 
# However, it first checks to see if the matrix inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function

#if X is a square invertible matrix, then solve(X) returns its inverse.


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached matrix inverse")
                return(i)
        }
        data <- x$get()      
        i <- solve(data, ...)
        x$setinv(i)
        i
}
