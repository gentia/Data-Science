## These two functions cache the data for a matrix given 
## and then after an initial calculation of the inverse, cache it so that it can be called upon later 

## This function has 4 nested functions that set the matrix data and caches the data inputed

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


## This function, after an initial calculation of the inverse based off the data from the previous function, 
## caches the answer of the inverse matrix 

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
