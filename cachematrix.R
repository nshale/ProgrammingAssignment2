makeCacheMatrix <- function(x=matrix()) {
  
  # Take the values and create a matrix  
  m <- NULL
  set<- function(y) {
    x <<- y
    m <<- NULL
  }
  # cache the matrix values for processing in second function
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x) {
  # call subfunction to solve for inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not null, solve for inverse and return
  data <- x$get()
  m< solve(data)
  x$setinverse(m)
  return(m)
}

