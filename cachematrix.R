#cacheMatrix.R

#Examples:
#
# A <- matrix(c(4,0,0,4), 2, 2)
#    > A
#          [,1] [,2]
#    [1,]    4    0
#    [2,]    0    4
# Acache <- makeCacheMatrix(A)
#
#   > Acache$get() 
#         [,1] [,2]
#    [1,]    4    0
#    [2,]    0    4
#
#   > Acache$getinverse()
#   NULL
#
#   > cacheSolve(Acache)
#   calculating inverse on the fly
#          [,1] [,2]
#    [1,] 0.25 0.00
#    [2,] 0.00 0.25
#
#   > cacheSolve(Acache)
#   getting cached data
#         [,1] [,2]
#   [1,] 0.25 0.00
#   [2,] 0.00 0.25



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #stores inverse of matrix
  set <- function(y) {
     x <<- y
     inv <<- NULL
  }
  get <- function() x  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the 
## cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating inverse on the fly")
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
