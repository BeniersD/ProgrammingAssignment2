##
## Usage examples:
## > my_matrix<-makeCacheMatrix(A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE))
## > d<-cacheSolve(my_matrix)
## > d
##	       [,1]    [,2]   [,3]
##	[1,] 0.0625  0.0625  0.125
##	[2,] 0.6875 -0.3125 -0.625
##	[3,] 0.2500  0.2500 -0.500
##
## > my_matrix_noinv<-makeCacheMatrix(A <- matrix( c(3,4,6,8), nrow=2, byrow=TRUE))
## > d<-cacheSolve(my_matrix_noinv)
##    Error in cacheSolve(my_matrix_noinv) : Inverse matrix doesn't exists!
##
## > my_matrix_notvalid<-makeCacheMatrix(c(3,4,6,8))
##    Error in makeCacheMatrix(c(3, 4, 6, 8)) : Please enter a valid matrix!
##

##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
         ## Initialize invx to NULL
         invx <- NULL
         ## Check whether the argument x is a valid matrix  
         if (!is.matrix(x)) { 
                  stop("Please enter a valid matrix!") 
         } 
         
         ## Setter function - set the value of the vector 
         set <- function(y = matrix()) {
                  x <<- y
                  invx <<- NULL
         }
         
         ## Getter function - get the value of the vector
         get <- function(){ 
	   		x
	   } 
         
         ## Setter function - set the value of inverse
         setinverse <- function(inverse){ 
			invx <<- inverse
	   }
         
         ## Getter function - get the value of inverse
         getinverse <- function(){
			invx
         }

         ## Return value
         list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
         invx <- x$getinverse()
         
         ## Check if invx is set already
         if (!is.null(invx)) {
                  ## Inverse matrix exists
                  message("getting cached inverse matrix data")
                  return(invx)
         }
         
         ## Get the matrix
         data <- x$get()

         ## Check if the inverse matrix exists
         if (det(data) == 0L){
		stop("Inverse matrix doesn't exists!") 
	   }
         ## Get the inverse matrix using the solve function
         invx <- solve(data, ...)
         ## Set the inverse
         x$setinverse(invx)
         ## Return value
         invx
}
