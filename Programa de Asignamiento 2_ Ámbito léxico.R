

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix and reset the inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set the inverse cache
  setInverse <- function(solve) 
    inv <<- solve
  
  # Function to get the inverse from the cache
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Get the inverse from the cache if available
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # If the inverse is not in the cache, compute it and store it in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

}
