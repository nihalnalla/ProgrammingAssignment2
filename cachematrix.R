## the first function makeCacheMatrix() takes a matrix input and 
## performs a set of actions, the output of which is a list which is 
## an input to the second function cachesolve() 

## makeCacheMatrix takes a matrix and sets its value and makes 
## its inverse in the environment it was created in.
##it will also return the data using get and getinv functions

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      #setting the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      
      #setting the inverse of the matrix
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## this takes the output of the above function and checks if the inverse
## operation is performed. if the inverse is not found then it performs
## the inverse and sets it in the 1st function using setinv() fn created 

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
      
}
