## Put comments here that give an overall description of what your
## functions do

# The funcion creates a matrix that can cache its inverse

## Write a short comment describing this function

# Creating a matrix object that retrieves  the inverse of its own

makeCacheMatrix <- function(x = matrix()) {
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
         getinverse = getinverse)}



## Write a short comment describing this function

# Computes the inverse of the special matrix returned by makeCacheMatrix
# if inverse is already calculated then retrieves the inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Using cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m}
