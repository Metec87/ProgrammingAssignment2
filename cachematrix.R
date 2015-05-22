# Computing the inverse of a matrix can be a time consumming computation. 
# Thus, it is important to cache the inverse of a matrix instead of 
# computing it repeartedly.The two functions below will be used to 
# cache the inverse of a matrix.

# The first function, makeCacheMatrix, will create a list containing a 
# function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
        x <<- y
        inv <<- NULL
  }
  
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set=set, get=get, 
        setinverse= setinverse, getinverse=getinverse)

}


# This second function will compute the inverse of the matrix. It will first
# check if the inverse of the matrix has been computed, if so, it will
# get the inverse of the matrix from the cache and skip the computation.
# Otherwise, it will compute the inverse of the matrix and set it value in the
# cache via the setinverse function

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse
  
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
  
}
