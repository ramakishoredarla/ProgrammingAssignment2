## Functions that cache the inverse of a matrix 
## use the following example to test the function

##> source("cachematrix.R")
##> m <- makeCacheMatrix(matrix(c(1,2,3,4),c(2,2)))
##>cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) { 
    x <<- y 
    i <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inv) i <<- inv 
  getinverse <- function() i 
  list( 
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse 
  ) 
}


## Calculate the inverse of the special "matrix" created with the above 
## function, reusing cached result if it is available 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() 
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
  } 
  m <- x$get() 
  i <- solve(m, ...) 
  x$setinverse(i) 
  i 
}
