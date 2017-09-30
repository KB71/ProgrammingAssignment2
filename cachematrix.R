## Matrix inversion caching using two functions
## so that we don't have to compute the inverse again...

## The function "makeCacheMatrix" creates a special
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matr <- NULL
    set <- function(y) {    
        x <<- y              
        matr <<- NULL            
    }
    get <- function() x
    setInMatr <- function(InMatr) matr <<- InMatr
    getInMatr <- function() matr
    list(set = set, get = get,
         setInMatr = setInMatr,
         getInMatr = getInMatr)
}

## This function "cacheSolve" takes a matrix "x"
## and returns its inverse.
## If the inverse has been calculated, it simply returns it.
## If not, it calculates it and then returns it.

cacheSolve <- function(x, ...) {
    matr <- x$getInMatr()              
    if(!is.null(matr)) {           
        message("Returning cached data")  
        return(matr)               
    }
    data <- x$get()             
    matr <- solve(data, ...)      
    x$setInMatr(matr)         
    matr              
}

# Testing:
# my_m <- matrix(1:4, nrow = 2)

# cach_m <- makeCacheMatrix(my_m)
# cacheSolve(cach_m)

