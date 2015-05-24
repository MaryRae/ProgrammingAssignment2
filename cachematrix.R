#makeCacheMatrix creates a special "matrix" object that can cache its inverse
#e.g. e<-makeCacheMatrix(rbind(c(1, 2, 3), c(2, 5, 3),c(1, 0, 8)) )
#then e$get() will return the full matrix
#use cacheSolve to save the inverse to the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#e.g.
#> e<-makeCacheMatrix(rbind(c(1, 2, 3), c(2, 5, 3),c(1, 0, 8)) )
#> cacheSolve(e)
#[,1] [,2] [,3]
#[1,]  -40   16    9
#[2,]   13   -5   -3
#[3,]    5   -2   -1
#the second time you run cacheSolve(e), it will return the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
