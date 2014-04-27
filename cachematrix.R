## A function to check for result in cache or calculate result
## in this case, inverse of matrix provided.
## A cache to calculate and store matrix inverse.

## using this function a cache can be created for calculating
## inverse of the matrix provided with the function call

makeCacheMatrix <- function(x = matrix()) {
  
    iv <- NULL
    set <- function(y) {
      x <<- y
      iv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) iv <<- solve
    getinverse <- function() iv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  

}


## in this function the cache is called and result of inverse 
## returned is already in cache otherwise the inverse is calculated.
## the matrix first needs to be 'set'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
