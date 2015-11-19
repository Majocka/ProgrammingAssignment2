## The code explores the idea of Lexical coding in two simple functions
## that will: 1. create an object to store (and cache) matrix and its inverse 
##            2. compute the inverse of matrix

## 1.
## makeCacheMatrix creates an object - list with 4 functions 
## to manipulate the properties of the object (not to perform computation) 
## - for easy "housekeeping" and maintenance of code
## so when the function is called it will be assigned to an object
## and computation on that object will be performed by cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
    ## Creates a list object and stores matrix 'x' and its inverse. 
    
    inv <- NULL
    set <- function(y) { # if wishing to change the object
        x <<- y          # will cache new value of x 
        inv <<- NULL     # and reset the cache of the inverse as NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve # here cacheing inverse
    getinv <- function() inv
    list(set = set,
    	 get = get,
         setinv = setinv,
         getinv = getinv)
}

## 2.
## cacheSolve makes either use of cached inverse (first part of function)
## or compute the inverse (second part of function) and calls x$setinv() 
## to cache it for quicker access in subsequent iterations

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {  # executed if cached
        message("getting cached data")
        return(inv)
    }
    
    print('executing first and last time') 	# if not cached
    data <- x$get() 						    
    inv <- solve(data, ...)                 # calculates the inverse
    x$setinv(inv)                           # to cache it for later quick use
    inv
}


