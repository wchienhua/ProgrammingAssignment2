## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function design for matrix inversion.
# The following two functions are used to cache the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
# The function blow returns the inverse of the matrix.
# It will check whether the inverse has already been computed.
# If it has been computed, it gets the result and skips the computation.
# Ohterwise, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


# Example 
x = rbind(c(1,-1/2), c(-1/2,1))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)
