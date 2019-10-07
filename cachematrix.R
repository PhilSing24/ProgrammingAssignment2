## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function (x=matrix()) {
  
  invMx <- NULL
  set <- function(y) {
    x <<- y
    invMx <<- NULL
  }
  
  get <- function() x
  setInv <- function(inv) invMx <<- inv
  getInv <- function() invMx
  
  setInv <- function(inv) invMx <<- inv
  getInv <- function() invMx                     
  list(set = set, get = get,
       setInv = setInv, 
       getInv = getInv)
  
}

cacheSolve <- function(x, ...) {
  invMx <- x$getInv()
  if(!is.null(invMx)) {
    message("getting cached data")
    return(invMx)
  }
  data <- x$get()
  invMx <- solve(data, ...)
  x$setInv(invMx)
  invMx
}



A <- matrix( c(5, 1, 0,
               2,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

 

x <- makeCacheMatrix(A)
y <- cacheSolve(x)
z <- cacheSolve(x)