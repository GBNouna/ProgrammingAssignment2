## The 2 functions bellow can be used to cache the inverse of a matrix
## functions do

## makeCacheMatrix generates a list that creates to properties (like) similar to object oriented programming
##  set the value of the matrix
##  get the value of the matrix

## setinv, set the value of inverse of the matrix
## getinv, get the value of inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## When cacheSolve function is executed, it does the following
## verify is the inverse of the matrix had been already calculated and cached inv  property created in makeCacheMatrix)
## if calculated already than ruturn the cache result
## if the property is null,IE, the inverse of the matrix was not calculated then is uses the solve function to calculatate the inverse 
## the inverse is assigned to the inv property (setinv) and return the result


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
 
  if(!is.null(m)) {
        message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
	
}

## important conciderations

## as requested by the instructor the code above do not verify if the matrix is square, if neither the value of it determinent not equal to 0


## example

## v <- c(4,3,6,8) ##create a vector
## x <- matrix(v,2,2) ## make it a matrix 2 by 2 square matrix , if we run det(x), the dterminent of the matrix is not 0, it's inversible
## xx <- makeCacheMatrix(x) ## cache the matrix in the property get

## xx$get() ## verification

## cacheSolve(xx) ## First time, the inverted matrice is not cached 
## cacheSolve(xx) ## second time we notice the print ,getting cached data



