## Assignment 2
## Set of a pair of functions to calculate the inverse of a matrix, and cache the results to avoid repeating computation.

## makeCacheMatrix creates a list of functions to set and get a matrix, and to set and get the inverse of the matrix.
## Input:  matrix() object.
## Output: list() object with 4 functions (set, get, setInverse, getInverse)
##         The first 2 set and get the matrix, and the other 2 set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve calculates the inverse of a matrix. It caches the results to avoid repeating computation.
## Input: a "Matrix" object (the output of makeCacheMatrix) and an optional list of arguments to be passed to the solve() function.
## Output:matrix() object (the inverse)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setInverse(inv)
       inv
}
