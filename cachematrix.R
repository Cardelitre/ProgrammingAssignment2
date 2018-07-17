

## makeCacheMatrix initializes an object with a matrix, the placeholder for the 
## inverse of this matrix (inv) and 4 functions set(), get(), setinv(), and getinv()
## to access and modify these 2 data objects

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## cacheSolve takes a makeCacheMatrix object as argument, check if a cached inverse
## exists and if not calculates and caches the inverse in the object

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
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
