## WLE20190302
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # Initiates a function to set the value of the matrix
    m <- NULL # the inverse will be stored as m
    set <- function(y) { 
        x <<- y # set the value of the matrix
        m <<- NULL # clear the old m from the cache
    }
    
    print(x) # print the value of the matrix
    
    get <- function() x # Get the value of the matrix
    setInverse <- function(solve) m <<- solve # Set the value of the inverse to m
    getInverse <- function() m # Get the value of the inverse 
    
    list(set = set, get = get, # list the four functions.... why?
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`

cacheSolve <- function(x, ...) {
    m <- x$getInverse() # fetches the inversed matrix from the cache
    
    print(m) # print the value of the inversed matrix
    
    if(!is.null(m)) { # Determine whether the matrix has already been cached, i.e. cache is not empty
        message("getting cached inverse matrix") 
        return(m) # returns the value of the inverse matrix
    }
    data <- x$get() # If the matrix has not yet beed inverted get the value for the new matrix
    m <- solve(data, ...) # Invert the new matrix
    x$setInverse(m) 
    m # Return the inverse of the new matrix 
    
}


Data = matrix(c(rnorm(4)), 2, 2) # Create a 2 by 2 matrix random numbers 
Matrix <- makeCacheMatrix(x = Data) # Apply the first function to the test data
Inverse <- cacheSolve(Matrix) # Apply the second function to the test data 


B <- matrix(c(1,2,3,4),2,2)
