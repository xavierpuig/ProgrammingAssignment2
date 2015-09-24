## Put comments here that give an overall description of what your
## functions do
# The functions create a matrix object with cache containers for both the matrix
# and its inverse, and provide a function to estimate the inverse matrix and
# store it in the provided containers


## Write a short comment describing this function
# This function creates a matrix object with a list of functions:
#       1) 'setMtr': stores the matrix 
#       2) 'getMtr': returns the stored matrix
#       3) 'setInvMtr': stores the inverse of the matrix
#       4) 'getInvMtr': returns the inverse of the matrix
# The function doesn't create nor compute the inverse of the matrix
#
# Example usage:
#       a <- matrix(c(1,5,7,3,1,8,5,1,3),3,3)   # define matrix
#       b <- makeCacheMatrix(a)                 # creates matrix object
#       b$getMtr()                              # retrieves matrix
#       b$setMtr(a)                             # alternative method to 
#                                               # define matrix
#       b$setInvMtr(x)  # stores matrix 'x' as the inverse matrix
#                       # but it doesn't check whether it really is
#       b$getInvMtr()   # retrieves the stored invers matrix
makeCacheMatrix <- function(x = matrix()) {
        # Save current matrix from formal argument 'x' to Mtr variable
        Mtr <- x
        # Set inverse matrix to NULL: override previous stored values of 'Inv'
        # To estimate inverse matrix function 'cachesolve' is needed
        Inv <- NULL
        
        # Nested function to allow setting a different matrix
        # Inverse matrix is deleted from cache when a new matrix is set
        # To estimate inverse matrix function 'cachesolve' is needed
        setMtr <- function(y){
                Mtr <<- y       # New matrix, stored in parent fun
                Inv <<- NULL    # Setting inverse matrix to null in parent fun
        }
        # Nested function to retrieve the current matrix
        getMtr <- function() Mtr
        
        # Nested function, stores inverse matrix to parent fun
        setInvMtr <- function(x) Inv <<- x
        
        # Nested function, retrives invers matrix from parent fun
        getInvMtr <- function() Inv      
        
        # Set list of nested functions within current function
        list(setMtr = setMtr, 
             getMtr = getMtr,
             setInvMtr = setInvMtr, 
             getInvMtr = getInvMtr)
}


## Write a short comment describing this function
# This function needs a matrix object created with 'makeCacheMatrix' to operate
# It checks whether the inverse matrix is stored in 'makeCacheMatrix's cache:
# returns it in case it exists, or estimates it if it doesn't.
# In case it has to estimate it, it stores the result in the cache. 
#
# Example usage:
#       cacheSolve(b)           # will return the inverse matrix of 'a' (comes
#                               # from previous function example)
#       a%*%cacheSolve(b)       # test: output is the identity matrix if the 
#                               # inverse has been properly assessed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Retrive inverse matrix from cache (NULL will be obtained if it 
        # doesn't exist)
        invm <- x$getInvMtr()
        # Check whether inverse matrix is already stored in cache within
        # function 'makeCacheMatrix' environment
        # If so, returns the inverse matrix
        if (!is.null(invm)){
                # Print message to let user know inverse matrix has been
                # retrieved from cache
                message("Retrive inverse matrix from cache")
                # Print inverse matrix using 'return' (instead of e.g.'print')
                # because 'return' will cause the function to cease here.
                # Instead of a return, the rest of the function could have
                # been enclosed in an 'else{}' condition
                return(x$getInvMtr())
        }
        # Estimate inverse matrix and save result in cache:
        # 1) retrieve original matrix
        m <- x$getMtr()
        # 2) estimate invere matrix
        invm <- solve(m)
        # 3) store inverse matrix in cache (within 'makecahematrix')
        x$setInvMtr(invm)
        # Print result
        invm
}

