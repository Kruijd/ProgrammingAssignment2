# makeCacheMatrix is the function that can be used to cache the results of a calculation on a matrix
# the results are stored in the 'm'variable in the environment of the function


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(abc) m <<- abc
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##################################################################

#cacheSolve is a function that calculates the inverse matrix of a given input.
#whenever the result of the calculation is alerady available in cache, it returns that.
# if it is not, it wil calculate the icerse matrix and return the result.

cacheSolve <- function(x, ...) 
{
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    #return(m)
    m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

###################################################################
# for testing purposes:

# define the matrix
ma<- matrix(rnorm(16),4,4)
ma
# execute the MakeCacheMatrix function to put the ma patrix into a.
a<-makeCacheMatrix(ma)

# execute cacheSolve a first time (not chached)
cacheSolve(a)
# execute cacheSolve second time, not the result comes from cahce
cacheSolve(a)


