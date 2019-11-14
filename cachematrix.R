## The "makeCacheMatrix" uses its argument to set and get the value of a special matrix,
## and then it sets and gets the inverse of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
                   
                   matrix_inverse <- NULL
                   
                   set_matrix <- function(y) {
                     x <<- y
                     matrix_inverse <<- NULL
                   }
                   get_matrix <- function() x
                   set_inverse <- function(inverse) matrix_inverse <<- inverse
                   get_inverse <- function() matrix_inverse
                   
                   list(set_matrix = set_matrix, get_matrix = get_matrix,
                        set_inverse = set_inverse, get_inverse = get_inverse)
                   
                   
}


## The "cacheSolve" function first checks whether the inverse matrix has previously
## been calculated. If it has not (matrix_inverse is NULL), the "cacheSolve" calculates
## the inverse of the matrix set by "makeCacheMatrix". Otherwise, the output is the
## cached object.


cacheSolve <- function(x,...) {
  
              matrix_inverse <- x$get_inverse()
              if (!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
                
              }
              data <- x$get_matrix()
              matrix_inverse <- solve(data,...)
              x$set_inverse(matrix_inverse)
              matrix_inverse
}

