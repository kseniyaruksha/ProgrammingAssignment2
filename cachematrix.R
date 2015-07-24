## The first function "makeCacheMatrix" is the parent environment which defines
## and then lists four functions: 
## 1)"set" -  for caching initial matrix(x),which also can be used to change x later  
## 2)"get" -  for calling initial matrix (x) from the environment
## 3)"setinverse" -  for solving (making an inverse of initial matrix) if the input matrix was provided  
## 4)"getinverse" -  for calling an inverse of the input matrix (x) from the parent environment 

makeCacheMatrix <- function(x = matrix()) {
  a <- matrix() ## creates an empty matrix
  set <- function(y){ ## caches initial matrix (x) if it was provided
    x <<- y 
    a <<- matrix() 
  }
  get <- function() x 
  setinverse <- function(solve) a<<- solve 
  getinverse <- function() a ## calles matrix "a" (either empty or provided with inverse)
  list (set = set, get=get, setinverse = setinverse, getinverse = getinverse) ## lists all available functions 
}


## The function "cacheSolve" provides cached matrix inverse from the parent 
## environment (using listed functions), except cases where there was no 
## input data in the first function. In this case "cacheSolve" creates data set 
##"data" and makes an inverse with the help of listed functions from parent environment 
## "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse() ## looks for cached inverse 
  if(!is.na(a)) { 
    message('getting cached data')
    return(a) 
  }
  data <- x$get() ## shows input matrix 
  a <- solve(data,...)
  x$setinverse(a)
  a ## shows matrix inverse
}
