## The first function, makeVector creates a special "vector", which is really a list containing a function to 1) set the value of the matrix, 2) get the value of the matrix, 3) set the value of the mean, and 4) get the value of the mean. The second function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## return : a list containing functions to
  ##   1) set matrix
  ##   2) get matrix
  ##   3) set inverse
  ##   4) get inverse
  ## list : input to cacheSolve() function
  inv = NULL
  set = function(y){
    ##'<<-' assigns value to object in
    ##'environment different from current one
    x <<- y
    inv <<- NULL
  }
  get = function()x
  setinv = function(inverse) inv <<- inverse
  getinv = function () inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## x : output of makeCacheMatrix
  ## return is the inverse of original matrix input to makeCacheMatrix() function
  
  inv = x$getinv()
  
  ## if inverse already calculated
  if (!is.null(inv)){
    #get it from cache and skips computation
    message("getting cache data")
    return(inv)
  }
  ## calculates the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  ## sets value of inverse in cache via setinv function
  x$setinv(inv)
  
  return(inv)
}

