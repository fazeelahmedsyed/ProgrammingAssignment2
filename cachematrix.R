## makeCacheMatrix creates a cached list of matrices and their inverses. 
## cacheSolve calculates the inverse of an invertible matrix.

## makeCacheMatrix takes in arguments for creating a matrix and then stores
## the created matrix in a cached list. The matrix can be called by '$get()',
## and altered by '$set()'. The cached inverse of the matrix (after cacheSolve is run)
## can be called by '$getinverse' or assigned a user defined value by '$setinverse()'.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        set <- function (y = matrix()) {
                x <<- y
                inv <<- NULL} 
        
        get <- function (){
                x}
        
        setinverse <- function(i) {
                inv <<- i}
        
        getinverse <- function() {
                inv }
        
        list <- list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes a matrix as an argument. This matrix should have already been 
## run by makeCacheMatrix() function or should be of the form 'makeCacheMatrix()'.
## cacheSolve evaluates if the inverse of the matrix is available in cached list or 
## or if the matrix is a new computation. If it is available the function returns 
## the cached value after the message "retrieving cached inverse" or calculates a new value
## and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)}
        else{
                mat <- x$get()
                inv <- solve(mat)
                
                x$setinverse(inv)
                inv}
}