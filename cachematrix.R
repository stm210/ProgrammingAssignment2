## Coursera [R] Programming Assignment 2
## Script inverts a matrix utilizing cache_
## if available to save time 
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
            
            setSolve <- function(solve) m <<- solve
            getSolve <- function() m
            
            list(set = set, get = get,
                 setSolve = setSolve,
                 getSolve = getSolve)
        }


## Part 2: cacheSolve function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 
        m <- x$getSolve()
      
        if(!is.null(m)) {
          message("Getting cached data.")
          return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
       
        m  
  
}
