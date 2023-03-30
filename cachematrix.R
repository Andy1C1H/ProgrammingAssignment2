#makeCacheMatrix Which is a function that takes in a
# that we already define it uses get,set, setinverse and getinverse
#to get and set the matrix and get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set inverse to be to null i.e so it has 0 length
  set <- function(y) {#set value of matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get matrix x
  setinverse <- function(solve) m <<- solve#set the inverse
  getinverse <- function() m#retrieve inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve gives the inverse of a special matrix
#using the solve function
# by checking to see if the inverse of the matrix has 
# been calculated before. If it has print the 
#"getting cache data and print the stored value,
# if it hasn't compute inverse, print it and store it


cacheSolve <- function(x, ...) {
  m <- x$getinverse()#get the inverse of x store it as m
  if(!is.null(m)) { #if there is a value for m i.e. an inverse for the matrix
    message("getting cached data")#tell us you are retrieving it
    return(m)#return the retrieved result the inverse of the matrix
  }
  data <- x$get()#get the matrix and store it as 'data'
  m <- solve(data, ...)# Return a matrix that is the inverse of data as m
  x$setinverse(m)#store m
  m#print m
}
