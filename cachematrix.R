## These two functions allow us to cache to result of a function, in this particular case, the function solve(), in 
## order to save our system from lengthy calculations. 
## Solving a matrix will require indeed an high computing power, especially for larger ones: 
## these two function will store the result and provide the already calculated output everytime that we ask for it, 
## avoiding to compute the calculation again from scratch.


## MakeCacheMatrix - This function stores a matrix in the x argument. 
## The matrix can be superposed to the empty x with the function set(). The function get() prints the matrix.


makeCacheMatrix <- function(x = matrix()) {
  mxinv <- matrix(, nrow(x), ncol(x))
  mxinv <- NULL        
  set <- function(mx)
  {
    x <<- mx
    mxinv <<- NULL
  }
  
  get <- function() x
  setslv <- function(mx) 
    mxinv <<- mx
  getslv <- function() mxinv
  list( set = set, get = get, setslv = setslv, getslv = getslv)     
  
}

## CacheSolve - This function will take the object defined in the previous function and return the value 
## of the operation that we commanded (here the function solve()). 
## The if operator checks whether we already had stored the result of the operation for the first time. 
## If this is the case, the message is printed, indicating that the result is being taken from cached data

cacheSolve <- function(x, ...) {
  mxinv <- x$getslv()
  if (!is.null(mxinv))
  { print("retrieving value from cache")
    return(mxinv)
  }
  data <- x$get()
   
   ## Calculate the inverse using Solve
  mxinv <- solve(data)
  
  x$setslv(mxinv)
  mxinv
  
  ## Return a matrix that is the inverse of 'x'
  
}

## latest version