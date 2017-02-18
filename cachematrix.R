## This code create inverse of matrix and cache the result first time
## so if next time same operation is performed the result is fetched from cache rather than recomputing 
## and thus saving computation time

## this function create a matrix using the arguments passed and
## has getter and setter function for retrieving matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inv varibale as NULL
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        ## return the matrix
        get <- function() x 
        ## setinv of matrix to inv variable
        setinv <- function(solve) inv <<- solve
        ## return inverse of function stored in inv variable
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## this function is used to retrieve the inverse of matrix
## if inverse is already computed it fetched from cache else compute

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## check if inv received is not NULL i.e inverse already computed
        if(!is.null(inv))
        {
                ## print the message
                message("Getting cached data as inverse already computed")
                ## return inverse of matrix and exit from execution
                return(inv)
        }
        ## get matrix from makeCacheMatrix using get function
        matrix_data <- x$get()
        ## calculate inverse using solve function
        inv <- solve(matrix_data)
        ##set inverse to inv variable using setinv function
        x$setinv(inv)
        
        ## return the inverse matrix
        inv
}
