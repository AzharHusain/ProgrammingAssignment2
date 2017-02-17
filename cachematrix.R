## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv() <- function(solve) inv <<- solve
        getinv() <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv))
        {
                message("Getting cached data as inverse already computed")
                return(inv)
        }
        matrix_data <- x$get()
        inv <- solve(matrix_data)
        x$setinv(inv)
        inv
}
