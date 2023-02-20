#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#    list containing functions

#        set the value of the matrix
#        get the value of the matrix
#        set the value of the inverse of matrix 
#        get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}

#testmat = matrix(rnorm(10000), nrow=100, ncol=100)
#chk <- makeCacheMatrix( x= testmat )

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), 
#             then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

#cacheSolve(chk)
