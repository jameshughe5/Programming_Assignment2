## These functions compute the inverse of a square matrix. The first function calculates
## and creates a cache of the inverse. The second checks to see if the inverse has been cached
## and if it hasn't it solves for the inverse and caches it. 

## This function creates a special matrix that 1) sets the value of the matrix 
## 2) get the value of the matrix 3) set the inverse of the matrix 4) get the
## inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function (y) {
            x <<- y
            inverse <<- NULL
      }
}
get <- function() x
setinverse <- function(sol) inverse <<- sol
getinverse <- function() inverse
list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)

## This function checks to see if the inverse has already been calculated. If it has, 
## the inverse is retrieved from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse via the
## setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse
      if(!is.null(inverse)) {
            message("retrieving cache")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}

