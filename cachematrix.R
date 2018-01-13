## The next function is creating a special "matrix", which really is a list, containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the matrix
## - get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL  # setting the inverse
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Here I am calculating the inverse of the "special matrix"
## First it checks if the inverse has been calculated before, 
## if yes, then pulls if from the cache via getinverse
## and skips the calculation. 
## Otherwise it calculates the inverse, and sets the value of the mx inverse 
## in the cache via the setinverse 

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if (!is.null(i)) {
          message("pulling data from cache")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}


## Example for testing
## Creating an invertible matrix
m<-matrix(runif(1e6,1,10),1e3,1e3)

## creating the "special matrix"
m1 <- makeCacheMatrix(m)

## first calculating the inverse
start_time <- Sys.time()
cacheSolve(m1) #the inverse of m is calculated
finish_time <- Sys.time()
print(c("execution time: ", finish_time - start_time))

## next, pull it from cache
start_time <- Sys.time()
cacheSolve(m1) #the inverse is returned from cache
finish_time <- Sys.time()
print(c("execution time: ", finish_time - start_time))
