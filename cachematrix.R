## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invM<-NULL
  
  ## store matrix and init inverse
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  ## get the Matrix
  get <- function() {
    x
  }
  
  ## set inverse 
  setInverse <- function(inverse) {
    invM <<- inverse
  }
  
  ## get inverse
  getInverse <- function() {
    invM
  }
    
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## if matrix X's inversion is cached, return it. 
# otherwise, solve and cache the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # check the cache
  invM <- x$getInverse()
  if(!is.null(invM)) {  
    message("getting cached data")
    return(invM)
  }
  
  ## get the matrix data
  data <- x$get()
  invM <- solve(data, ...)
  
  ## cache the inverse
  x$setInverse(invM)
  invM
}


## test case
## x<-makeCacheMatrix(matrix(c(1.835044e+01,8.392485e-04,8.392485e-04,4.093558e-07),2,2))
## cacheSolve(x)

