## Two functions are included here.  The first makeCacheMatrix
## should be called on a square matrix.  The second cacheSolve uses the
## return of makeCacheMatrix to form an inverse if none exists or
## returns the cached inverse if it is present.



## makeCacheMatrix provides four functions
## get to retrieve the value of the matrix, set to insert a new matrix,
## getinverse and setinverse to retrieve and insert the values of
## the inverse respectively.  The value for the inverse is cached in
## the variable inv.
## Usage : m<- matrix(c(4,2,7,6), nrow = 2)
##           u <- makeCacheMatrix()


makeCacheMatrix <- function(x = matrix()) {
    
    if (any (is.na(x))) message("Matrix contains NA") 
    inv <- NULL
   
    set <- function(y) {
        if (class(y) != "matrix" | any(is.na(y))) {
            message("Not a matrix or matrix contains NA value")
            return()
        }
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
    
    
}


## cacheSolve uses the output of makeCacheMatrix.  It retrieves
## the inverse if one has been cached or calls the 
## function solve() to create 
## one and then sets it into the makeCacheMatrix object.
## An error is returned if the matrix is singular.
## Usage: cacheSolve(u) where u is the output of makeCacheMatrix.

cacheSolve <- function(x, ...) {
         
    ## Return a matrix that is the inverse of x
    inv <- x$getinverse()
    if( !is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...) 
    x$setinverse(inv)
    inv
}

