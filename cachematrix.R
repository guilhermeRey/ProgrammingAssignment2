##  makeCacheMatrix(this)
##    this: a matrix() object
##  The function wraps a matrix object that is capable of:
##    get/set it's matrix;
##    cache it's inverse matrix
##    return it's inverse matrix, that is cached
makeCacheMatrix <- function(this = matrix()) {
  this_inverse <- NULL
  
  get <- function () this
  set <- function (newMatrix) {
    x <<- newMatrix
    this_inverse <<- NULL # flush cached attribute
  }
  
  setInverse <- function (inverse) this_inverse <<- inverse
  getInverse <- function () this_inverse
  
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve(x, ...)
##  x: a matrix object
##  returns: x inverse matrix
## The function ensures that x is solved and cached, returning it.
##
cacheSolve <- function(m, ...) {
  inverse <- m$getInverse()
  # So it is null? Let's solve and cache it!
  if(is.null(inverse)) {
    message("INFO: Calculating and caching...")
    matrix <- m$get()
    m$setInverse(solve(matrix, ...)) 
  }
  
  m$getInverse() # always return the solved matrix :)
}
