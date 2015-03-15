# This R file provides a pair of functions to calculate the inverse of a matrix and 
# cache the value so that it can subsequently be used without repeating the computation.

# makeCacheMatrix creates an object which is a list of functions 
# for storing and reading back a matrix and its inverse
# Args:
# x - a square matrix that is invertible
# Returns:
# a list of functions set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {

    #initializes the variable inv.x that will hold the inverse of the matrix
    inv.x <- NULL   
    
    # define a function that allows you to update the matrix x
    set <- function(y) {    
        x <<- y
        inv.x <<- NULL # if x is updated, anything previously stored in inv.x is cleared
    }
    
    # define a function that returns the current value of x
    get <- function() x    
    
    # define a function that sets inv.x to the value of the argument passed to it
    setinv <- function(inv) inv.x <<- inv   
    
    #define a function that returns the current value of inv.x
    getinv <- function() inv.x
    
    # create an object that is a list of all the above functions
    # this is the output of the function makeCacheMatrix
    list(set = set, get = get,   
         setinv = setinv,
         getinv = getinv)
    
}


# cacheSolve returns a matrix that is the inverse of 'x'
# The inverse is only calculated once and then stored using makeCacheMatrix()
# and on subsequent calls to cacheSolve, the inverse matrix is looked up, using
# the functions created by makeCacheMatrix 
# Args:
# x - an object created by makeCacheMatrix (which holds a stored square matrix)
# ... - other arguments for solve()
# Returns:
# inv.x - the inverse of the matrix stored in x

cacheSolve <- function(x, ...) {

    # first check if that makeCacheMatrix object already has a stored inverse matrix
    inv.x <- x$getinv()   
    if(!is.null(inv.x)) {   # if it does, it just returns that stored inverse matrix
        message("getting cached data")
        return(inv.x)
    }
    # if no inverse is stored, then pull out the original matrix stored in x
    orig.matrix <- x$get()
    
    # and then calculates the inverse using solve
    inv.x <- solve(orig.matrix, ...) 
    
    # store the calculated mean back in the makeCacheMatrix object for future use
    x$setinv(inv.x)  
    
    # and finally return the newly calculated inverse
    inv.x  
    
}
