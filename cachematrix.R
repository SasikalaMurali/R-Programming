## comments functions do

## gets the matrix from the caller and sets inverse value in matInv
## gets the matrix from the caller and sets value in x
## returns matInv and x to the caller according to the call

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setMatInv <- function(inverse) matInv <<- inverse
  getMatInv <- function() matInv 
  list(set = set, get = get, setMatInv = setMatInv , getMatInv = getMatInv)
}


## gets the matrix data and computs inverse of it and based on cached data, it is retrived.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
mInv <- x$getMatInv()
  if(!is.null(mInv)) {
    message("getting cached result")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setMatInv(mInv)
  mInv
}

## Checking the program
## mat <- matrix(rnorm(25),5,5)
## cacheMat <- makeCacheMatrix(mat)
## cacheSolve(cacheMat)

##          [,1]        [,2]         [,3]       [,4]        [,5]
## [1,] -0.23867123 -0.33943987  0.254092112 -0.5425942 -0.01392323
## [2,]  0.20129369 -1.03676271 -0.489072527 -0.3073473  0.37789959
## [3,] -0.19346076 -0.50222956 -0.252326046 -0.5683810  0.66284101
## [4,]  0.30425051  0.19844426  0.001104857 -0.2830803 -0.01324993
## [5,]  0.04171303  0.01479722 -0.491164919 -0.4650824 -0.24401821

