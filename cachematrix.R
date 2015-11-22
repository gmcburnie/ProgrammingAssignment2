## A pair of functions. When executed with a given matrix, will calculate the inverse of the matrix on first run.
## The inverse matrix is cached and returned.
## Subsequent executions of the second function on an object will return the cached inverse without recalculating. 

## Takes matrix as input; returns object containing list of functions. 
## Object encodes input matrix (x) and cached inverse matrix (m) if it has been set (by cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
    ## clear 'm' should function be called on object with new matrix.
    m <- NULL
    ## Set 'x' as input matrix and clear cached output variable 'm' in enclosing environment. 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Return formal argument matrix 'x'.
    get <- function() x
    ## Set 'm' across enclosing environment with value in argument 'solve' (cached output).
    setsolve <- function(solve) m <<- solve
    ## Return cached output variable 'm'.
    getsolve <- function() m
    ## Call each function and return contents of functions in a list.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Takes object with list of functions including input matrix, as produced by makeCacheMatrix.
## On first run, calculates and returns inverse matrix; caches value as variable 'm' in input object.
## Returns cached inverse matrix value (m) on subsequent runs for an object.

cacheSolve <- function(x, ...) {
    ## Calls 'getsolve' function from input object; sets 'm' as cached inverse matrix if already exists.  
    m <- x$getsolve()
    ## If 'm' is not empty, message and return contents of 'm' (cached inverse matrix).
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Call 'get' function. Sets 'data' as the object's input matrix.
    data <- x$get()
    ## Set 'm' as inverse value of input matrix 'data' using 'solve'.
    m <- solve(data, ...)
    ## Use setSolve function to set value of 'm' across the object; output value. 
    x$setsolve(m)
    m
}