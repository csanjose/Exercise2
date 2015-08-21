# Functions provided;
# creatematrix; Creates a square matrix;
# makeCacheMatrix; Special function with te following methods:
    # set, Set value of matrix;
    # get, Get value of matrix;
    # setinverse, Set value of inverse;
    # getinverse, Get value of inverse;
    # checkinverse, Check if inverse is correct;
# cacheSolve; Returns cached inverse of matrix, if does not exist, then calculates it

creatematrix <- function (min, max, size) {
    # Function that creates a square matrix
    # between min and max with size row and size columns
    valor=as.integer(runif(size^2, min, max))
    matrix(valor, nrow=size, ncol=size)
}

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
    ## Return a matrix that is the inverse of 'x'
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
