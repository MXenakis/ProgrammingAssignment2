## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "Matrix" object. It basically returns a list of functions to 
## manage this matrix object such as setting the object with a matrix, inverting the matrix and getting the aforemetioned results
## (getting the input matrix and getting the inverted matrix)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## sets the input matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x ## returns the input matrix
        setInverse <- function(solve) m <<- solve ## computes the inverse matrix
        getInverse <- function() m     ## stores the inverse matrix
        list(set = set, get = get,     ## returns a list of functions pertaining to the "Matrix" object
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function calculates the inverse of a matrix. If the input is the same it doesn't compute again the inverse
## but just returns what is stored in the cache (m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() #get the matrix from the "matrix" object
        
        if(nrow(data) != ncol(data)){ # Check if the matrix is a square matrix.
                print("The matrix must be square!")
                return(-1) # If not the solve function will return an error return -1 instead
        }
        
        if(det(data)!= 0){ #if not 0 there is an inverse matrix see ?det for more
                m <- solve(data, ...)
                x$setInverse(m)
        }else{
                print("There is no inverse matrix!")
                return(NA) #return 0 if matrix was not inversed
        }
        
        ##Return the inversed matrix
        m
}
