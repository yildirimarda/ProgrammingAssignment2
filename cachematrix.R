## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## For that purpose, two functions are written below.
## makeCacheMatrix function is used to create a special object that stores a matrix 
## and cacheSolve function is used to cache inverse of that matrix. 

##  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse of the matrix
##  4.get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
