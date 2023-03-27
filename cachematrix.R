

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


# cacheSolve gives the inverse of a special matrix
# by checking to see if the inverse of the matrix has 
# been calculated before. If it has print the stored value,
# if it hasn't compute inverse, print it and store it


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { #if there is a value for m
    message("getting cached data")#tell us you are retrieving it
    return(m)#return the retrieved result
  }
  data <- x$get()
  m <- solve(data, ...)# Return a matrix that is the inverse of 'x'as m
  x$setinverse(m)#store m
  m#print m
}