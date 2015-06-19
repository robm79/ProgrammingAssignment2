## Function to cache the inverse of a matrix for speedy reference later
## Properties: get/set, getinverse/setinverse


## Creates a "special" matrix that can hold a matrix and its inverse in cache
## Properties to get/set the matrix and get/set the inverse
makeCacheMatrix <- function(x = matrix()) {
        # create temp NULL m
        # define set
        # define get
        # define setinverse
        # define getinverse
        # define list
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Returns the inverse matrix of x
## First checks to see if inverse has been cached in memory
## If not, calls makeCacheMatrix to create inverse and cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # create temp m by getting inverse
        # check if m is null, if not print 'getting' and return m
        # else set m by creating the inverse of a matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
