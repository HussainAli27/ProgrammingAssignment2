## The makeCacheMatrix Function stores a Matrix and the cached copy of its Inverse
#THe inverse is calculated by cacheSolve function which check the availabilty of 
#cached inverse as a precondition, hence reducing the calculations.



## Store a matrix and its inverse and returns a list to get/set Matrix/its Inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  
  # return list of set/get functions
  list(set = set, get = get,
       setInverse = setInv,
       getInverse = getInv)
  
}


## Calculates the Inverse if not stored in cache and returns the Inverse

cacheSolve <- function(x, ...) {
        
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
