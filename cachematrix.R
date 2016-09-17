## Example about using lexical scope to improve performance


## Function makeCacheMatrix....
## This function receive a squared non-singular matrix
## and calculate its inverse matrix.
## This function also stored the inverse matrix in 
## a free variable in cache to speed performance.
## The function returns a list with 4 members which
## can be invoked as methods.

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(solve) inv<<-solve
  getinv<-function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Function cacheSolve....
## This function receive a list returned by the 
## makeCacheMatrix function and returns the inverse matrix.
## If the inverse is already calculate, the function returns 
## the result store in the cache; if not makes the entire 
## calculation of the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
