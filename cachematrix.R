## Thus file includes two functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  ## creates the function which sets the matrix and cleans the cache 
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  ## creates the function which gets the matrix
  get <- function() x
  ## creates the function which sets the inverse 
  setinverse <- function(inverse) invs <<- inverse
  ## creates the function which gets the inverse
  getinverse <- function() invs
  ## returns the list of required functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  #checks if the inverse exsists in the cache and returns it if yes
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  # calculates the inverse if it is not in cache
  invs <- solve(data, ...)
  x$setinverse(invs)
  ## returns the inverse of the matrix x
  invs      
}
