## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix", which is really a list containing a function to
## 1. set the value(s) of the matrix elements
## 2. get the value(s) of the matrix elements
## 3. set the inverse matrix 
## 4. compute and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set() function definition
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        ## get() function definition
        get <- function() x
        ## definitions for setinverse() and getinverse()
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        ## create list of previously defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If yes, it retrieves the cached inverse, skipping the 
## calculation; else, it computes the inverse and uses setinverse() to store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Check to see if inverse exists. If so, return it and skip computation
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        # If the inverse is not already cached, compute it, cache it, and display it.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
