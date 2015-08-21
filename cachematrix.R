# Functions provided;
# creatematrix; Creates a square matrix;
# makecachematrix; Special function with the following methods:
    # set, Set value of matrix;
    # get, Get value of matrix;
    # setinverse, Set value of inverse;
    # getinverse, Get value of inverse;
    # checkinverse, Check if inverse is correct;
# cachesolve; Returns cached inverse of matrix, if does not exist, then calculates it

creatematrix <- function (min, max, size) {
    # Function that creates a square matrix
    # between min and max with size row and size columns
    valor=as.integer(runif(size^2, min, max+1))
    matrix(valor, nrow=size, ncol=size)
}

makecachematrix <- function(x = matrix()) {
    
    # Some basic error handling
    if(class(try(solve(x), silent=T))=="try-error") {
    return(message('Matrix must be square and numeric'))
    }
    
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
            message("Correct!")
        }
        else {
            message("Incorrect, check your data")
        }
    }
  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, checkinverse = checkinverse)
       
}

cachesolve <- function(x, ...) {
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
