## These function will create matrix, and calculate the inverse of matrix.
## If inverse of matrix has been calculated, function cacheSolve will just get
## the matrix from previous results.

## This function creates a special ?€œmatrix?€? object that can cache its inverse.
## x means original matrix, ivsM means inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {  
 
  # initialize
  ivsM <- NULL  
  set <- function(y){
    x <<- y
    ivsM <<- NULL
  }
  
  # pass the matrix(called by cacheSolve)
  get <- function() x
  
  # set results for caching
  set_ivsM <- function(ivs) ivsM <<- ivs
  
  # pass cached results(called by cacheSolve)
  get_ivsM <- function() ivsM
  
  # elements of func.objects
  list(set=set,get=get,set_ivsM=set_ivsM,get_ivsM=get_ivsM)
}

## This function computes the inverse of the special ?€œmatrix?€? returned
## by makeCacheMatrix above.
## If the inverse has already been calculated(and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.
## [WARNING: x means func.object, not a matrix itself(unlike func. makeCacheMatrix)]
cacheSolve <- function(x, ...) {
  
  # load inversed matrix from cache
  ivsM <- x$get_ivsM()
  if(!is.null(ivsM)) {
    message("getting cached data")
    return(ivsM)
  }
  
  # if it doesn't exist,
  # load original matrix(x$get()) and calculate(solve()) it.
  matrix<-x$get()
  ivsM<-solve(matrix,...)
  
  # save results in func.object(as "ivsM" cache)
  x$set_ivsM(ivsM)
  
  # return results
  ivsM
}

## to use :
##
## make func.object(in this case, "mt") and
## pass the original matrix(in this case, "mtr") >> setting completed
## then call func cacheSolve to calculate(pass the func.object, not a matrix)
## => in this case("mt", not "mtr")
##
## i.e.
## mtr<-matrix(c(2,3,4,1,7,2,4,8,2),nrow=3,ncol=3)
## mt<-makeCacheMatrix(mtr)
## cacheSolve(mt)
