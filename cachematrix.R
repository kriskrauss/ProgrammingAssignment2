## Cache results to avoid time-consuming computations
## so results can be returned from cache (if input has not changed)
## else compute and cache the results for reuse
## for computing inverse of an invertible matrix

## Creates a special "vector" -- a list contaning functions 
## to get and set the value of the vector
## as well as the inverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {

   ## initialize
   m <- NULL

   ## getters and setters

   ## set the matrix for which inverse needs to be computed
   set <- function(y) {
      x <<- y
      m <<- NULL
   }

   ## get matrix for which the inverse needs to be computed
   get <- function() x

   ## save in cache
   setinversematrix <- function(inversematrix) m <<- inversematrix

   ## retrieve from cache
   getinversematrix <- function() m

   ## return a list of functions available in this special vector
   list(set = set, get = get,
        setinversematrix = setinversematrix,
        getinversematrix = getinversematrix)
}


## Creates inverse matrix of the special "vector" created with above function
## if inverse matrix already calculated, returns output from cache
## else computes inverse matrix, stores it in the cache and returns output

cacheSolve <- function(x, ...) {

   ## check cache
   m <- x$getinversematrix()

   ## if in cache, return the inverse matrix
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }

   ## not in cache, compute the inverse matrix
   data <- x$get()
   m <- solve(data)

   ## store computed inverse matrix in cache
   x$setinversematrix(m)

   ## Return the inverse matrix
   m
}
