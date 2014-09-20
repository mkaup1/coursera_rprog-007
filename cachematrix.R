## Begin of function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  	m <- NULL
  	set <- function(y) {
    	x <<- y
    	m <<- NULL
  }
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}
##End of function makeCacheMatrix


##Begin of function cacheSolve
cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  if(!is.NULL(m)) {
   	message("getting cached data")
    	m
    	break
  }
  
  mydata <- x$get()
  
  m <- solve(mydata, ...)
  
  x$setinv(m)
  
  return(m)
}
##End of function cacheSolve