 ## The makeCacheMatrix function below creates a special matrix, 
  ## which is really a list containing a function to set the value
  ## of the matrix, get the value of the matrix, set the value of 
  ## the inverse, and get the value of the inverse.
  
  makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  get inverse <- function() m
  list(set=set,
       get=get,
       setinverse = setinverse,
       getinverse=getinverse)
  }
  

  ## The function below produces the inverse of the matrix created
  ## with the above function. It checks to see if the inverse has 
  ## already been calculated; and if so, it gets the inverse from
  ## the cache and skips the computation. If not, it calculates the 
  ## inverse of the data via the setinverse function.
  
  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
  }
  
    }
    ## Return a matrix that is the inverse of 'x'
  }
