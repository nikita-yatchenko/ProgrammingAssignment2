## Below are two functions that allow to make use of cache memory to better deal
## with potentially time-consuming operations, like computing the inverse of a
## matrix. We will take the advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object

## makeCacheMatrix is a function that creates a list of functions as its output, 
## allowing to: set the value of the matrix, get the value of the matrix, set the
## value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special matrix object, created
## using makeCacheMatrix function. It checks whether the iverse has already been calculated
## and stored in the object. If so, then it retrieves the inverse from the cache and returns
## it. Otherwise, it calculates the matrix inverse and sets the value of the inverse
## in cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
