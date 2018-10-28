## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    # function: cache matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # function: get matrix
    get <- function() x
    
    # function: cache inverse matrix
    setinv <- function(xinv) m <<- xinv  
    
    # function: get inverse matrix
    getinv <- function() m
    
    # create a list containing functions defined above
    list(set = set, get = get,  
         setinv = setinv,
         getinv = getinv)
    
}

## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

# How to run these functions:
# a = makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))   ## create and cache matrix
# cacheSolve(a)     ## compute and cache inverse of matrix
# cacheSolve(a)     ## get inverse from cache and print message "getting cached data"

