## The following functions create a matrix from a list of functions
## which set and get the value of a matrix as well as set and get
## the value of its inverse. cachesolve then checks to see if the 
##value of of the inverse is cached and if not it computes it returns it
## and caches it.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #create variable to store inverse matrix value (prevents potential scoping errors)
        inv_mat <-null
        
        set <- function(y){
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv_mat <<- inverse
        getinv <- function() inv_mat
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cachesolve retrieves the inverse matrix from the cachematrix function. 
## First, it checks to see if the cached value already exists, if not
## it calculates it and then caches the calculated value and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)){
                message("getting cached data")
                return(inv_mat)
        }
        #if value not cached get it.
        data <- x$get()
        #solve calculates inverse of a square matrix
        inv_mat <- solve(data,...)
        #store the value of the inverse in cache
        x$setinv(inv_mat)
        #return value
        inv_mat
}
