# Since matrix inversion is a costly computation, it makes sense to cache 
# the inverse of a matrix rather than compute it each time. Functions 'makeCacheMatrix' and 
# 'cacheSolve' enable caching the inverse of a matrix.

## This function creates a special "matrix" object (a list of functions) that can cache its inverse (getInverse).
## The initial matrix should be an invertible square matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache. Otherwise, the inverse is calculated by the 'solve' 
## function, and the value is set in setInverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

## TEST 
## =============================================================
mat <- matrix(c(10,3,4,5,12,5,6,11,4), 3,3) # create a 3*3 matrix
special_mat <- makeCacheMatrix(mat) # apply makeCacheMatrix
## =============================================================
# Getting initial matrix
# > special_mat$get()
#       [,1] [,2] [,3]
# [1,]   10    5    6
# [2,]    3   12   11
# [3,]    4    5    4
# 
# Since cache is not computed yet, getInverse returns NULL
# > special_mat$getInverse()
# NULL
## =============================================================
cache <- cacheSolve(special_mat) # caching the inverse (after computing it)
#             [,1]        [,2]       [,3]
# [1,]  0.06481481 -0.09259259  0.1574074
# [2,] -0.29629630 -0.14814815  0.8518519
# [3,]  0.30555556  0.27777778 -0.9722222

