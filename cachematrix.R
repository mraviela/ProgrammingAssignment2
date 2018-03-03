## Creates object/entity which stores input value (matrix) 
## and to-be cached inversed representation

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Creates object/entity which returns cached inversed representation if already calculated 
## or calculates it eagerly and caches it for futher usage


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

# Example

# A <- matrix( c(5, 1, 0,
#                3,-1, 2,
#                4, 0,-1), nrow=3, byrow=TRUE)
#
# cm <- makeCacheMatrix(A)
#
# cacheSolve(cm)
#        [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500

# cacheSolve(cm)
# getting cached data
#        [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500