## These functions allow caching the evaluation of the inverse of a matrix


## This function receives a matrix, x, saves its value and allows
## its modification (set function) and its lecture (get function)
## the inverse of this matrix is saved in the inv_m variable
## it can be set with the setinv function and can be read by the getinv function
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_x <<- inv
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function receives an object created by the makeCacheMatrix function
## If the inverse of x has been evaluated before, its value is returned  
## otherwise, the inverse is evaluated and its value is cached and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        inv_x
}
