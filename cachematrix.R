

## Function that makes a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function () x
    list(set = set, get = get,
        setinverse = setinverse, 
        getmean = getmean)
}


## Solves for the inverse of the matrix object created above. 

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
         }
    data <- x$get()
    i <- inverse(data, ...)
    x$setinverse(i)
    i
}
