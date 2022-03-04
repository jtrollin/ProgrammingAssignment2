## Put comments here that give an overall description of what your
## functions do

## This function will build a cache of the inverse of the 
## matrix that is passed in to it. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will return the inverse of a matrix passed in to it
## it will check the cache first (provided by the makeCacheMatrix method)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Results of the code running
# > tmp_mx <- matrix(1:4, nrow=2)
# > tmp_mx
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mx <- makeCacheMatrix(tmp_mx)
# > cacheSolve(mx)
# [1] "list"
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mx)
# [1] "list"
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
