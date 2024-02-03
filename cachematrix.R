## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) mat_inv <<- inv
  get_inv <- function() mat_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  mat_inv <- x$get_inv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$gt_inv()
  mat_inv <- solve(data, ...)
  x$set_inv(mat_inv)
  mat_inv
}
