################## Assignment: Caching the Inverse of a matrix #################
#
#  Functions:
#     makeCacheMatrix - creates a "matrix " object that can cachi its inverse
#        - contains functions for setting and getting the matrix,and the inverse of 
#          the same matrix
#
#     cacheSolve - uses the Solve function to compute the inverse of the input matrix

##################################################################################


## makeCachematrix returns functions that are used to 
## get or set a matrix, 
## get or set the inverse of that matrix
## makeCachematrix returns a list containing these functions 
## The functions can be accessed by the $ operator of the variable that
## makeCacheMatrix returns...e.g.
##  - 





makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
       x <<- y  
       inv <<- NULL
       message("Matrix set. Inverse set to null")
       print(x)
  }
  
  
  get <- function() {
    message("Matrix retrieved is:")
    x
  }
  setinv <- function(solve) inv <<- solve        
  getinv <- function() inv
  message("CacheMatrix functions loaded")
  return(list(set=set, get=get,
       setinv=setinv,getinv=getinv))
  
  
  
  ## cacheSolve does one of two things:
  #     1. If the matrix is already cached, it will return the cached values
  #     2. Otherwise, it will calculate the inverse and return that. 
  
 # example input for using this function:
  #   set.seed(100)
  #   a <- makeCacheMatrix(matrix(rnorm(16,mean=10,sd=3),nrow=4,ncol=4))
  #   cacheSolve(a) computes the inverse and stores it in the cache
  #   a$get() would return the original matrix
  #   a$get() would return the inverse matrix
  # a$get() %*% a$getinv() --- would return the identity matrix
  
}

cacheSolve <- function(x, ...) {
  message("Original Matrix:")
  inv <- x$getinv()
  if(!is.null(inv)) {
      message("inverse retrieved from cached data")

    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  message("inverse stored in cached data")
 inv
  
}