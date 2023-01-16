makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL  ##inverse as null
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ##get matrix x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
        ## this function is creating a matrix object to cache its inverse
        ## for the input

cacheSolve <- function(x, ...) { ##to get cache data
        s <- x$getsolve()
        if(!is.null(s)) {       ##check if inverse is null
                message("cooking up your result") ## trying to be quirky
                return(inv) ##returns inverse
        }
        data <- x$get()
        inv <- solve(data, ...)         
        x[setinv(inv)]
        inv
}
        ##cacheSolve is for figuring out the inverse of "makeCacheMatrix"