## The two functions create and cache the inverse of a matrix. 

## makeCacheMatrix is a function that creates(set) & gets(get) 
## a matrix and creates(setinverse) & gets(getinverse) the 
## inverse of the same matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL               ##initialise the variable that will store the inverse
  set<-function(y){     ##creates the matrix
    x <<- y
    m <<- NULL
  }
  get <- function()x    ##return the value of x in the current environment
  setinverse<-function(solve) m <<- solve     ##creates the inverse matrix
  getinverse <- function() m      ##gets the inverse matrix
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve is a function that calculates the inverse of a special matrix
## created by makeCacheMatrix. It first checks if the inverse is already 
## available: if it has already been calculated it gets the inverse from
## the cache, otherwise it calculates the inverse of the matrix and stores 
## it in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()   ##get the value of the inverse matrix
  if(!is.null(m)){      ## if it has already been calculated returns the inverse matrix cached
    message("getting cached data")
    return(m)
  }
  data<-x$get()         ## if the inverse matrix is not available in the cache calculates it
  m<-solve(data,...)
  x$setinverse(m)
  m
}


