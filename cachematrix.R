
# The codes return 5 functions:
# - the get function          -> get the matrix in makeCacheMatrix function argument "x"
# - the setmatrix function    -> store the matrix in the cache matrix variable "w" 
# - the get matrix function   -> get the cache matrix stored in the variable "w"
# - the setinverse function   -> store the cache inverse matrix to the variable "inv"
# - the getinverse function   -> get the cache inverse matrix stored in the variable "inv"

makeCacheMatrix <- function(x = matrix()) {
  get <- function() x
  setmatrix <- function(matr) w <<- matr
  getmatrix <- function () w
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list( get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse)
}

# First I check if there is a cache matrix and cache matrix inveres in the global environment
# the cache matrix should be in the "W" variable and the inverse in the "inv" variable
# if they do not exist I create them and assign NULL to them

# Then I check if the matrix to be computed and the cache matrix are identical.
# If so, I check if the cache invers is not equal to NULL and if not I get it and return it

# if "inv" is NULL or if the matrix are not identical:
# - I calculate the inverse
# - I store the invers into the variable "inv"
# - I store the matrix into the variable "W"
# - I return the "inv"
    

cacheSolve <- function(x, ...) {
  if(exists("inv") == FALSE) inv <<- NULL
  if(exists("w") == FALSE) w <<- NULL
  
  r1 <- x$get()
  r2 <- x$getmatrix()
  
  inv <- x$getinverse()
  
  if(identical(r1, r2)) 
  {
    if(!is.null(inv)) 
    {
      message("getting cached data")
      return(inv)
    } 
    else 
    {
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      w <- data
      x$setmatrix(w)
    }
  } 
  else
  {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    w <- data
    x$setmatrix(w)
  }
  
  inv
        ## Return a matrix that is the inverse of 'x'
}
