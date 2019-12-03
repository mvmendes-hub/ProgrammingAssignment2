## Assigment Coursera's Course - Week 3 - Creating a matrix “dest_a” that can cache its inverse
## We are assuming an invertible matrix supplied (always)
## Marcus Vinicius Mendes Pereira; email's address: mvmendes@gmail.com

makeCacheMatrix <- function(dest_a = matrix()) {
  mm <- NULL
  set <- function(source_a){
    dest_a <<- source_a
    mm <<- NULL
  }
  get <- function()dest_a
  setInverse <- function(inverse) mm <<- inverse
  getInverse <- function() mm 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(dest_a, ...) {
  ## Return a matrix that is the inverse of 'dest_a'
  mm <- dest_a$getInverse()
  if(!is.null(mm)){
    message("getting cached data")
    return(mm)
  }
  mat <- dest_a$get()
  mm <- solve(mat,...)
  dest_a$setInverse(mm)
  mm
}
