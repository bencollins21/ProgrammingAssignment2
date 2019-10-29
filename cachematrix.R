## These two functions combine to invert a matrix, as long as the matrix is invertible.

## The function below creates a special "matrix" object that can cache its inverse.
## It starts by initialising the two objects (x and m), with x already initialised in the function argument.
## Then it defines the four behaviours, based on mutator and accessor methods.
## set() assigns the input argument to the x object in the parent environment and
## assigns the value NULL to the m object in the parent environment - which clears the cache when x is reset.
## get() uses lexical scoping to retrieve x from the parent environment.
## setinv() sets the function needed and allows us to access m after setinv() completed.
## getinv() is similiar to get() by allowing us to retrieve m from the parent environment.
## list() then names each function and assigns them as an element within a list before returning to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function below can populate and/or retrieve the inverted matrix from an object of type makeCacheMatrix().
## It starts by initialsing the argument x and allowing the user to include additional arguments with the ellipsis.
## It then attempts to retrieve the inverted matrix.
## If the inverted matrix is cached it will then message will show followed by the inverted matrix.
## If m is NULL then the function gets the matrix from the input object and inverts it before returning the value of the inverted object.
## If the same input object is used again then it will select the same inverted matrix as it has been cached.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}