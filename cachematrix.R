## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve: takes a "cacheMatrix" and optional params
## and returns a stored solve result from the cacheMatrix or 
## calculates a result if not stored (result of solve)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached inverse...")
    return(inv)
  }  
  message("calculating matrix inverse...")
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}
