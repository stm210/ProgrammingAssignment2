## Coursera [R] Programming Assignment 2
## Script inverts a matrix utilizing cache to save time 
## Inputs: an invertable matrix
## Outputs: inverse of the matrix

## Part 1: set up the cache piece
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
               x <<- y
               m <<- NULL
            }
            get <- function() x
            
            setsolve <- function(solve) m <<- solve
            getsolve <- function() m
            
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
        }


## Part 2: cacheSolve function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 
        m <- x$getsolve()
      
        if(!is.null(m)) {
          message("Getting cached data.")
          return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
       
        m  
  
}
