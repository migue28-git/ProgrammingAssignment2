## The makeCacheMatrix and cacheSolve functions work in a complementary way.
## First you define a matrix in makeCacheMatrix to a variable, let´s call it "cacheMatrix".
## You pass "cacheMatrix" as a formal argument to cacheSolve to calculate it´s inverse
## and now, the inverse matrix is saved in makeCacheMatrix function until you use it with other matrix

## makeCacheMatrix is a special function that make a matrix and save it inverse in cache memory.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<-y
    m <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculate the inverse of the matrix that makeCacheMatrix did, and put it in the 
## cache memory of makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m))
  {
    return(x$getinverse)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
