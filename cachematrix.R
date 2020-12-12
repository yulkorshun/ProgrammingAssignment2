## The function takes a matrix, create the object that could be used to
## create inversion and store in cache. It generates the inversion and 
## store it in cache

## This function creates the object and  option for its modifications (set)
## also it creates caches for inversion

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL # initiation of inv
  set <- function(y){  # that funcion allow to change matrix value 
                       # without initiating new object
    x <<- y   #assign matrix y to matrix x in parent 
    inv <<- NULL # inv set to NULL in parent 
  }
  get <- function() x # passing argument x to other function
  setinversion <- function(inv1) inv <<- inv1 # cache of inv 
  getinversion <-function() inv  # gets values from cache inv
  list(set = set, get = get, 
       setinversion = setinversion, 
       getinversion = getinversion)
  # list return 'special vector' containing defined function,
  # that allows to use $ in order to call the property
}


## This function check for existed inversion in cache,
## and if it is not there , it is generate one, sends in cache
## and also return.
## if there is inversion in cache, it retrieves and return with
## message

cacheSolve <- function(x, ...) {
 inv <- x$getinversion() #checks for value in cache
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) # gets from cache
  }
  data <- x$get()#if it is no in cache,assign the
                # vector to new var data
  inv <- solve(data, ...) #calculate new inversion
  x$setinversion(inv) # sets it in cache
  inv    # return inversion to the parent       
}
