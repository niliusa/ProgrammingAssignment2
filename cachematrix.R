## The R code below has two functions: makeCacheMatrix and cacheSolve.  
## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## cacheSolve will decide whether to retrieve the cached inverse when it's already computed, or compute the inverse when it's not. 

## makeCacheMatrix has four functions: set; get; 
## setinverse to set the inverse of a matrix; 
## getinverse to call the data stored by setinverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## casheSolve function will search if the stored matrix inverse m is null or not. 
## If it's not null, then return m without computing the inverse. 
## If m is null, then use x$get to get the data, then compute the matrix's inverse using slove function, and use x$setinverse to set the new inverse , and return the inverse. 

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

