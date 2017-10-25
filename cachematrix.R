##list of functions allowing user to define and retrieve valus for the initial matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Finds the inverse of the matrix if it hasn't been cached before

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

#Example

mCM 	<- makeCacheMatrix()
x 	<- matrix(1:4,2,2)

mCM$set(x)

cacheSolve(mCM)

#repeat to show cache message
cacheSolve(mCM)
