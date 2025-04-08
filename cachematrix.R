## Functions that create a special matrix object and cache its inverse.

## Function that takes a matrix as input and builds a list of functions
## that together can set and retrieve both the original matrix x and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This variable will store the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # This will reset the cache when the matrix is changed
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Creates a function called cacheSolve, which takes the matrix x
## and optional other inputs, and attempts to get the inverse from
## a cache if it exists. Otherwise, it solves the inverse and stores
## it in the cache.

cacheSolve <- function(x, ...) {

  ## Return the inverse of the matrix x
  inv <- x$getInverse()
  
  if (!is.null(inv)) { # If inverse is already cached
    message("Now getting cached data...")
    return(inv)  # Return cached inverse and skip lines below
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setInverse(inv)       # Cache the inverse
  inv
}
