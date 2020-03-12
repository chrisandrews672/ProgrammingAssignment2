## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes a matrix as its input and places an inversion of that matrix into the cache for future use
# The cache is stored in the parent environment so that a partner function called cacheSolve can access it
# makeCacheMatrix first initialises m as NULL
# Then wipes previous cache record (in the parent environment) using set <<- to set m to NULL
# The function then defines three additional functions. The get assigns x, from the parent environment
# the setsolve also takes m from the parent and applies the function solve (to apply the inversion)
# the getsolve then retrieves the value
# the final list stores each function as a list and the partner cacheSolve accesses the appropriate function using the $ symbol

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

# The function takes the object from makeCacheMatrix function
# This function checks the cache for an existing inversion and if one exists then it needn't recalculate against the whole matrix
# This includes an elipsis which allows the necessary additional arguments for the function
# It begins with gathering the value of m from the parent environment
# If that value is not null then the cached version of m will be returned
# Otherwise the underlying data within x will be queried and the solve function will apply
# This returns the inverted matrix as the output

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
