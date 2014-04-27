## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function uses a closure mechanism to set values of a starting and
## inverse matrix. This function will be used by another function to
## actually manage these values.
## Any time that starting matrix is set, the inverse matrix is set to NULL,
## So that it needs to be calculated again.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    
    set <- function(y){
        x <<- y
        invm <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse){
        invm <<- inverse
    }
    
    getinverse <- function() invm
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Write a short comment describing this function
## Use the makeCacheMatrix function (passed in as an argument)
## to accomplish the behavior below:
## If inverse matrix value is cached, print a statement about getting cached
## data. Otherwise, compute inverse of a matrix with solve() and cache it.
## Show the inverse matrix value on the screen.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    m <- x$get()
    inverse <- solve(m)
    x$setinverse(inverse)
    inverse
}
