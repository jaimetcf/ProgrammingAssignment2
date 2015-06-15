# This function creates a matrix object which stores the 
# matrix passed as argument, and can also store a second 
# matrix which is supposedly the inverse of the first matrix.
#
# Both matrix stored objects are private to this function,
# in a sense that they can only be accessed from external 
# R environments through the list object returned by
# this function and the 4 functions defined within this
# function, like in the example below:
#
# m <- matrix(c(1,3,9,21,11,0,0,9,7),3,3)
# cm <- makeCacheMatrix(m)
# cm$get()
#
# where, this last line returns the stored matrix object
# 
# The 4 functions defined within makeCacheMatrix allow 
# storing and returning the 2 matrix objects 
makeCacheMatrix <- function(x = matrix()) 
{
  # Initializes object 'invx' which will be used to store
  # the inverse matrix of 'x'.
  invx <- NULL
  
  # Stores the y matrix passed as argument into object 'x'.
  # Also, resets the inverse matrix object, 'invx'. 
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  get <- function() x  # Returns the stored object matrix
  
  # Stores a supposedly inverse matrix of x, passed as
  # a argument 'inverse'
  setinverse <- function(inverse) invx <<- inverse
  
  # Returns the matrix stored in 'invx' object
  getinverse <- function() invx
  
  # Returns the list of functions that can be accessed
  # from external environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This funciton returns a matrix that is the inverse of the
# matrix 'x' passed as argument. This argument 'x' has to be a 
# "special matrix object" created with function makeCacheMatrix, above.
#
# Also the matrix stored in object 'x' has to be square and invertible.
#
# If it is the first time this function is executed, since
# the creation of 'x' matrix or since execution of function
# x$set(y), cacheSolve will calculate the inverse of 'x'
# and store it in 'x'. Othewise, cacheSolve will skip
# inverse calculation and simply return the inverse matrix
# already stored in 'x'.
cacheSolve <- function(x, ...)
{
  # Gets the inverse matrix stored in 'x'
  invx <- x$getinverse()
  
  # If the inverse is not NULL, what means it was already
  # been calculated and stored, returns it.
  if(!is.null(invx)) {
    return(invx)
  }
  
  data <- x$get()       # Gets the stored matrix in 'x' object
  invx <- solve(data, ...)     # Calculates the inverse matrix
  x$setinverse(invx)  # Saves the inverse matrix in 'x' object
  invx      # Returns the calculated and stored inverse matrix
}
