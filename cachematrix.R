## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix <- function(x = matrix()) {

##}


## Write a short comment describing this function

##cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
##}

# The inverse of a matrix
#Here are two functions to cache the inverse of a matrix
# It is assumed that the matrix supplied is always invertible.

#Function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #inverse property
    inver <- NULL 
    ## Method to set the matrix
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    ## Method the get the matrix
    get <- function() x
    setInverse <- function(inverse) inver <<- inverse
    getInverse <- function() inver
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Obtain the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getInverse()
    if (!is.null(inver)) {
        message("getting cached matrix")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setInverse(inver)
    inver
}