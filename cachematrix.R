## Here are a pair of functions that will help in efficiently calculating the inverse of a matrix.
## If an application needs to perform matrix inversion often, with the same source matrix,
## these methods will cache a previously calculated inversion to save processing time.

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Init member variables
    m <- NULL

    # Setter method
    # Must clear the cache in case the source matrix has changed.
    # An optimization would be to compare the new matrix with the old,
    # to see if we really need to clear the cache.
    # But this is probably not worth the effort, especially for a large matrix.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Getter method
    get <- function() {
        x
    }

    # Getter and setter for the inverse
    setinv <- function(invval) {
        m <<- invval
    }
    getinv <- function() {
        m
    }

    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function will efficiently calculate the inverse of a matrix
## by using the object created by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    # Diagnostics: This is not required by the assignment but for my own curiosity
    # See how long each code path takes
    stm <- proc.time()

    # See if a cached value already exists
    m <- x$getinv()
    if(!is.null(m)) {
        # Result was cached - return the previously calculated value
        message("getting cached data")
        message(proc.time() - stm)
        return(m)
    }

    # Nothing cached, calculate the inverse and return it
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    message(proc.time() - stm)
    m
}

