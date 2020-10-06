## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   ## inverse of matrix
    ## function to set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## function to get the matrix
    get <- function() x
    ## function to set matrix inverse
    setinverse <- function(inverse) i <<- inverse
    ## function get the matrix inverse
    getinverse <- function() i
    ## return the list of methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()    ## get inverse of matrix
    ## if inverse is already there then return cached inverse
    if(!is.null(i)) {      
        message("getting cached data")
        return(i)
    }
    ## else get the matrix
    data <- x$get()
    ## and compute the inverse
    i <- solve(data, ...)
    ## and set the inverse
    x$setinverse(i)
    i
}