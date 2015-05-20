# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function (inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Sample run:
##> r1 <- c(3, 1, 45)
##> r2 <- c(5, 6, 2)
##> r3 <- c(4, 6, 6)
##> x <- rbind(r1, r2, r3)
## > m = makeCacheMatrix(x)
## > m$get()
##   [,1] [,2] [,3]
##r1    3    1   45
##r2    5    6    2
##r3    4    6    6
## No cache in the first run
## > cacheSolve(m)
##            r1       r2        r3
## [1,]  0.07500  0.82500 -0.837500
## [2,] -0.06875 -0.50625  0.684375
## [3,]  0.01875 -0.04375  0.040625
## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##getting cached data
##           r1       r2        r3
##[1,]  0.07500  0.82500 -0.837500
##[2,] -0.06875 -0.50625  0.684375
##[3,]  0.01875 -0.04375  0.040625
## >