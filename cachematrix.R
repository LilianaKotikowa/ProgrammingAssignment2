## creates a special "matrix", which is really a list 
## containing a function to:

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function (y){
  x <<- y
  m <<- NULL
}
get <- function() x
setinv <- function(inv) m <<- inv
getinv <- function() m
list (set = set, get = get, 
      setinv = setinv, 
      getinv = getinv)
}


## calculates inverse of the special "matrix" created with function above

# first checks if inverse was already calculated
# if so, get() will get the inverse from the cache and skip computation
# Otherwise calculates inverse and sets the value to the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
