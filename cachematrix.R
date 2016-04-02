## The function creates the special "matrix" object.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## associates matrix object y to x in the parent environment
    x <<- y
    m <<- NULL
  }
  ## returns the matrix object
  get <- function() x
  ## sets the inverse matrix to cache
  setinverse <- function(inverse) m <<- inverse
  ## returns the cached matrix inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of a special matrix and sets it in the cache. 
## If it exists, it retrieves from the cache and displays a informative message about it
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## checks if the inverse exists in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #computes the inverse of the special "matrix"
  m <- solve(data, ...)
  ## and sets it in the cache
  x$setinverse(m)
  m
}