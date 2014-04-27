## Assignment: Caching the Inverse of a Matrix
## 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {      # Sets data in parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x       # Reads data from parent environment
  setinverse <- function(inverse) inv <<- inverse  # Sets inverse in parent environment
  getinverse <- function() inv   # Reads inverse from parent environment
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## It is assumed that the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()           #query the x vector's cache         
  if(!is.null(inverse)) {         #if there is a cache
    message("getting cached data") 
    return(inverse)               #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  inverse <- solve(data, ...)        #we actually compute them here
  x$setinv(inverse)                #save the result back to x's cache
  return(inverse)                           #return the result
}
