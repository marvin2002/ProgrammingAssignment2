
## There are two functions, the first one is makeCacheMatrix that initialize with a matrix x and a null matrix m,
## as a result  makes available  a list of four functions that may use cacheSolve 
## to return the inverse of a matrix set up in the first function.
## Then,  cachesolve verify if the inverse was already been calculated, 
## if so, it just retrieve it, otherwise make the math.


## Overall both function save some time at calculating the inverse of a reversable x matrix.

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


## cacheSolve gives the inverse of a matrix.

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

