# Assumption!
# "For this assignment, assume that the matrix supplied is always invertible."


## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix constains the following functions:
# 1. set(newmatrixvalue) - set the value of the matrix
# 2. get()               - get the value of the matrix
# 3. setinverse(inverse) - set the value of inverse of the matrix
# 4. getinverse()        - get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL
    set <- function(newmatrixvalue) {
        x <<- newmatrixvalue
        cachedinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedinverse <<- inverse
    getinverse <- function() cachedinverse
    list(   set = set, 
            get = get, 
            setinverse = setinverse, 
            getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse of the matrix.")
        return(inverse)
    }
    data <- x$get()
    calculatedinverse <- solve(data)
    x$setinverse(calculatedinverse)
    calculatedinverse
}

# Sample code:
# matrix = rbind(c(1, -1/4), c(-1/4, 1))
# cachedmatrix = makeCacheMatrix(matrix)
# cachedmatrix$get()
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00

# No cache in the first run
# cacheSolve(cachedmatrix)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

# Retrieving from the cache in the second run
# cacheSolve(cachedmatrix)
# getting cached inverse of the matrix.
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# 
# Retrieving straight from the matrix
# cachedmatrix$getinverse()
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667