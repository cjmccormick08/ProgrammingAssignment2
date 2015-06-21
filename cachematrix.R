## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes in a matrix 'x' and caches the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}


## cacheSolve takes returns the inverse of the cached input 'x'

cacheSolve <- function(x, ...) {
       
  m <- x$getsol()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsol(m)
  m
  
  
  }
