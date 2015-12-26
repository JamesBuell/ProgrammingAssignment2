## The makeCacheMatrix and cacheSolve functions below are adapted directly from the assignment examples makeVector and cacheMean. 


## makeCacheMatrix creates a list whose function:
# 1 sets the matrix value, 
# 2 gets the value, 
# 3 sets the value of the inverse of the matrix, and 
# 4 gets that value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
#1 set the matrix value
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }

#2 get the value
  get <- function () x

#3 set value of the inverse  
  setinverse <- function(inverse) inv <<- inverse

#4 get the inverse's value
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse
       )
  
}


## cacheSolve is adapted from the assignment example cacheMean


## cacheSolve returns the inverse of the matrix. It
## 1 checks whether the inverse already exists
## 2 if so, it skips the computation and gets the result
## 3 if not, it computes the inverse and then sets its value in the cache using the setinverse function
## 4 output the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

#1 get inv - does it already exist?
  inv <- x$getinverse()
#2 if it does, get it and return the result
    if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
    }
# 3 otherwise, use R's generic "solve" function to create the inverse
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
# 4 output the result
  inv
}

  ## ("solve" is not described in R. Peng's book or course videos but is mentioned in the assignment instructions: "Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse."
  ## "solve" is also described here among other places: http://www.statmethods.net/advstats/matrix.html)
  ## (Full disclosure: I gained further insight into this assignment from googling similar work that has been published online by prior course students.) 

