## Put comments here that give an overall description of what your functions do

# Test variable

# This function "makeCacheMatrix()" create an object that has 4 methods
# 1 - $get : allows you to retrieve matrix data you provided when using the set 
# function
# 2 - $set : allows you to set new matrix data
# 3 - $setinverse : force the result for the inverse (without speciality 
# computing 
# it)
# 4 - $getinverse : retrieve the inverse (if it has been computed in an other 
# function (see function 2))

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(my_inverse) i <<- my_inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}

x <- makeCacheMatrix()



# This function "cacheSolve()" allows to set the inverse of the matrix by using 
# the solve() 
# function. If the data already exist, it tells you what it is by getting it 
# from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
} 

# Now I test on new data
matrix_test <- matrix(data=c(1:4),nrow = 2, ncol = 2)
x <- makeCacheMatrix(matrix_test)
cacheSolve(x)
x$getinverse() # tell you the inverse of the matrix I set ()
