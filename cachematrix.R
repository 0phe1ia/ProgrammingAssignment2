makeCacheMatrix <- function(x = matrix()) {
      #This function, makeCacheMatrix creates a special "matrix", with functions to
      #1. set the value of the matrix
      #2. get the value of the matrix
      #3. set the value of the inverse of the matrix
      #4. get the value of the inverse of the matrix
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

cacheSolve <- function(x) {
      #This function inverses a matrix of the special "matrix" created with the above function. 
      #However, if the inversed matrix already exists, it gets the matrix from the cache and skips the computation. 
      #Otherwise, it inverses the matrix using solve() 
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setmatrix(m)
      m
}