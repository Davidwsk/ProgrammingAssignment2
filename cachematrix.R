## To avoid recalculation of Inverse of a Matrix by caching the Inverse
## makeCacheMatrix: create special Matrix object which cache/keep the Inverse of the Matrix
## cacheSolve     : return the Inverse of the Matrix, use cached Inverse of the Matrix if available

## Special Matrix object with following function
## set       : set the value of Matrix
## get       : get the value of Matrix
## setinverse: set the value of Inverse of Matrix
## getinverse: get the value of Inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the Inverse of a Matrix
## if no cached Inverse of the Matrix available, calculate and save the result
## if no cached Inverse of the Matrix available, use cached Inverse of the Matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
