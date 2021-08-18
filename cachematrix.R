## A function that sets the matrix and initializes functions to be able to store 
## and retrieve its value and inverse later

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
  
}


## A function that gets the inverse of a matrix produced by the previous
## and stores it for retrieval later

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
          message("Getting cached inverse")
          return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
