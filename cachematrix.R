## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some 
#  benefit to caching the inverse of a matrix rather than computing it 
#  repeatedly.
## The following pair of functions are used to cache the inverse of 
#  a matrix.

## The first function, makeCacheMatrix creates a special "matrix" that can 
#  cache its inverse.
## It creates a list containing functions to
#  1.set the value of the vector
#  2.get the value of the vector
#  3.set the value of the mean
#  4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned 
#  by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
## The function assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
    message("getting cached data.")
    return (invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setInverse(invMatrix)
  invMatrix
}

## Sample
#  x <- matrix(c(1,2,2,1),2,2)
#  invMatrix <- makeCacheMatrix(x)
#  invMatrix$get()
#  [,1] [,2]
#  [1,]    1    2
#  [2,]    2    1
## No cache
#  cacheSolve(invMatrix)
#  [,1]       [,2]
#  [1,] -0.3333333  0.6666667
#  [2,]  0.6666667 -0.3333333
## Retrieve from cache
#  cacheSolve(invMatrix)
#  getting cached data.
#  [,1]       [,2]
#  [1,] -0.3333333  0.6666667
#  [2,]  0.6666667 -0.3333333
