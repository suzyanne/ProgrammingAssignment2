## makeCacheMatrix is modeled after makeVector, but it caches a vector and its inverse
## instead of a vector and its mean.  

## makeCacheMatrix creates a cached matrix or a cached inverse (no calculations)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                         ## i is the inverse of a matrix.
  set <- function(y) {              ## set "overrides" or "sets" a matrix to the argument.
      x <<- y
      i <<- NULL
  }
  get <- function() x                  ## tells you what matrix is currently stored
  setinverse <- function(inverse) i <<- inverse      ## sets i to a given inverse
  getinverse <- function() i           ## tells you what inverse is stored
  
  ## list of functions within makeCacheMatrix.  You can call
  ## individual functions from the list using ...$get, ...$set, etc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a cached inverse or solves for inverse if none is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if (!is.null(i))  {
    message("getting cached inverse data")
    return(i)
  }
  data <- x$get()  ##putting matrix to solve in data
  i <- solve(data) ##solving for inverse
  x$setinverse(i)  ##setting inverse in cache for future reps
  i                ##displaying inverse
}
