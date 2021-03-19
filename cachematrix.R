## Functions to compute and cache the inverse of a matrix

## First creates special matrix x_inv

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  
  set <- function(y){
      x <<- y
      x_inv <<- NULL
  }
  
  get <- function() x
  # use function solve to get the inverse
  setinv <- function(solve) x_inv <<- solve
  
  getinv <- function() x_inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## computes inverse of the special matrix x_inv

cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  
  data <- x$get()
  # when the second argument (b) of solve is missing, it computes the inverse
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
