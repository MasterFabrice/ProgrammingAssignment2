
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    w <<- NULL
    inv <<- NULL
    }
  get <- function() x                                 # gives the object
  setmatrix <- function(matr) w <<- matr              # put the variable into w
  getmatrix <- function () w                          # gives the w
  setinverse <- function(inverse) inv <<- inverse     # put the variable into inv
  getinverse <- function() inv                        # gives the inv
  list(set = set, 
       get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  r1 <- x$get()
  r2 <- x$getmatrix()
  inv <- x$getinverse()
  if(identical(r1, r2)) {
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    } else {
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      w <- data
      x$setmatrix(w)
    }
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    w <- data
    x$setmatrix(w)  
  }
  inv       
        ## Return a matrix that is the inverse of 'x'
}
