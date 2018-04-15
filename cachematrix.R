
## Function designed to solve matrix and store reult to cache; if already solved extract from cache

## Function makeCacheMatrix creates list of possible data movements

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Determines and returns status of cache, if necessary solves matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    print("getting inverse from cache")
    return(i)
  }
  data <- x$get()
  print(class(data))
  solve(data, ...)
  x$setinverse(i)
  i
}