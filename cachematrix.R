## Put comments here that give an overall description of what your
## functions do

## All my explanation will bring object-oriented concepts and paradigmas.
## Where the object contains attributes and methods.

## makeCacheMatrix will create a list with these "methods" {'set','get',
## 'setinverse','getinverse'}, this list will contain two attributes {'x','m'};
## x : Will store the matrix to be inverted.
## m : Will store the inverse matrix.
## set : Must set x to the matrix which you apply.
## get : Return the x attribute inside the list.
## setinverse: Must receive the solution through cacheSolve function.
## getinverse: Return the inverse matrix if already solved. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

## cacheSolve function will verify if inverse was already solved, if not, 
## will solve and cache into m attribute through setinverse method.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
    }
    print("is null")
    data <- x$get()
    print("Getting the data")
    m <- solve(a = data)
    x$setinverse(m)
    m
}