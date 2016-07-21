## The functions described below output the inverse of a matrix.
## But finding the inverse is a very costly aware in terms of the time complexity.
## So buy using the concept of 'cache' we can improve the performance.
## The first function uses a 'super assignment' operator, i.e. "<<-" 
## to assign a value to a variable outside of the current environment.


## This function returns a "matrix" containing a variable and other functions.
## It actually returns a list.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function actually returns the inverse of a matrix contructed 
## by the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
