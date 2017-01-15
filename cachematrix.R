## This function creates a special "matrix" object that can cache its inverse
## First, create a function that starts with null arguement

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(matrix=y) {
    x <<- y
    m <<- NULL
  }
  
  ## Here, get the value of the inverse 
  ## Use solve function to calculate the inverse
  get <- function() x
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
