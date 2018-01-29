## These two functions cache the data given and then after an initial calculation of the inverse, cache it 

## This function creates the cache matrix and caches the data inputed

makeCacheMatrix <- function( x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
    
  }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function, after an initial calculation of the inverse, caches the answer 

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
