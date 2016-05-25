## m <- NULL sets the inverse of a matrix to NULL as a placeholder for future value 
## set <- function (y) { x <<-y ; m<<-NULL } defines a function to set the vector, x, to a new vector, y, and resets the inverse, m, to NULL.
# get<- function () x returns original matrix
# setinverse <- function(solve) m <<- solve sets the inverse, m, to function solve [to get the inverse of a matrix]
# getinverse <- function () m returns matrix inverse


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
m <- NULL 
  set <- function (y) 
  {
    x <<-y
    m<<-NULL
  }
  get<- function () x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function () m
  list (set = set, get =get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then cacheSolve should retrieve the inverse of cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  if(!is.null(m)) 
  {
    message ("getting cached data")
    return (m)
  }
  data <- x$get ()
  m <- solve(data, ...)
  x$setinverse (m)
  m
}
