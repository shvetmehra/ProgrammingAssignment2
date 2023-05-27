## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize the inverse matrix as NULL
  inverse <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y  # Assign the new matrix to the parent environment
    inverse <<- NULL  # Reset the cached inverse
  }
  
  # Method to get the matrix
  get <- function() {
    x  # Return the matrix
  }
  
  # Method to set the inverse of the matrix
  setInverse <- function(inv) {
    inverse <<- inv  # Assign the inverse to the parent environment
  }
  
  # Method to get the inverse of the matrix
  getInverse <- function() {
    inverse  # Return the inverse
  }
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special matrix object returned by makeCacheMatrix()
cacheSolve <- function(x) {
  # Check if the inverse is already computed and cached
  if (!is.null(x$getInverse())) {
    message("Getting cached inverse")
    return(x$getInverse())  # Retrieve the cached inverse
  }
  
  # If the inverse is not cached, compute it using solve()
  data <- x$get()  # Get the matrix data
  inv <- solve(data)  # Compute the inverse
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}



## Write a short comment describing this function

# Create a matrix object
mat <- makeCacheMatrix(matrix(c(2, 4, 1, 3), nrow = 2))

# Get the matrix
mat$get()
# Output:
#      [,1] [,2]
# [1,]    2    1
# [2,]    4    3

# Compute the inverse and cache it
cacheSolve(mat)
# Output:
#      [,1] [,2]
# [1,] -1.5  0.5
# [2,]  2.0 -1.0

# Retrieve the cached inverse
cacheSolve(mat)
# Output:
# Getting cached inverse
#      [,1] [,2]
# [1,] -1.5  0.5
# [2,]  2.0 -1.0

# Set a new matrix
mat$set(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse for the new matrix
cacheSolve(mat)
# Output:
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

## Return a matrix that is the inverse of 'x'
}
