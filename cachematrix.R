# A cached version of the matrix inversion operation for quadratic matrices.
# Avoids to recompute the inverse if it was computed before

# Make a cached version of the provided matrix. Use this version for inverse
# calculation "cacheSolve" to make use of caching.
makeCacheMatrix <- function(x = matrix()) 
{
  # the cached version of the inverse matrix
  invM <- NULL
  
  # setter function for the cached matrix
  set <- function( y ) 
  {
    # bind new value
    x <<- y
    # reset cached inverse matrix
    invM <<- NULL
  }
  
  # getter function chached matrix
  get <- function() x
  
  # setter function for inverse matrix
  setInv <- function( v ) invM <<- v
  
  # getter function for inverse matrix
  getInv <- function() invM
  
  # return list
  list( set = set, get = get, setInv = setInv, getInv = getInv )
}

# invert a matrix or return previously calculated result if cached
cacheSolve <- function(x, ...) 
{
  # get cached result
  invM <- x$getInv()
  
  # result cached?
  if (!is.null(invM))
  {
    message( "getting cached data" )
    
    # return cached value and exit function 
    return (invM)
  }
  
  # otherwise calculate and cache result
  
  # calculate inverse matrix
  invM <- solve( x$get())
  
  # cache inverse matrix
  x$setInv( invM )
  
  # return result
  invM
}
