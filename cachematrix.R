makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(x) inversa <<- x
  
  getinverse <- function() inversa
  
  checkinverse <- function() {
    if (identical(inversa,solve(x))) {
      print("Correct!")
    }
    else {
      print("Incorrect, check your result")
    }
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, checkinverse = checkinverse)
       
}

cacheSolve <- function(x, ...) {
  inversa <- x$getinverse()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinverse(inversa)
  inversa
}
