## Function to complete assignment 2
## Create function to cache a matrix's inverse and 
## another function to actually obtain the inverse
## Written by I. Aldasoro

## (1) The first function takes a matrix element as input and returns 
## a list with 4 items (set, get, setinverse, getinverse)

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

## (2) The second function actually computes the inverse
## First it checks whether the inverse is already cached to the matrix
## If yes, it reports it, if not it calculates it and caches it to 
## the matrix

cacheSolve <- function(x, ...) {
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

# Example usage:
# testmat <- makeCacheMatrix(matrix(1:9,3))
# testmat$get()
# testmat$getinverse()
# cacheSolve(testmat)
# testmat$getinverse()
# cacheSolve(testmat)
