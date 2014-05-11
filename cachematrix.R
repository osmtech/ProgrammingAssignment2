## Put comments here that give an overall description of what your
## functions do

## this function returns pseudo-object for handling cache,
## since R is sadly not an object-oriented language we have to resort to this trick with lists
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # sets deep cache with  <<- operator
  setmatrix <- function(matrix) m <<- matrix
  # gets from cache (m)
  getmatrix <- function() m
  #returns list of functions instead of object 
  list(set = set, get = get,
       setcache = setmatrix,
       getcache = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check cache
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get original data
  data <- x$get()
  message("solving matrix")
  # resolve and set cache
  m <- solve(data)
  x$setcache(m)
  #return 
  m
}

## USAGE
mx <- matrix(seq(1:4),2)
x <- makeCacheMatrix(mx)
cacheSolve(x)
cacheSolve(x)
cacheSolve(x)

# OUTPUT
#> cacheSolve(x)
#solving matrix
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(x)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(x)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5