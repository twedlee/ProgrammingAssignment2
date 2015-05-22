## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix<-function(x = matrix()){
 
  m <- NULL
  set <- function(y){  
    x <<- y #set the matrix
    m <<- NULL
  }
  
  get <- function() x #get the matrix
    
  setmatrix <- function(solve) m <<- solve(x) ##matrix is solved and saved
  getmatrix <- function() m #matrix is stored in memory
  
  list(set=set, get=get, setmatrix=setmatrix, 
       getmatrix=getmatrix) #list what've you got.
  
  
}

cacheSolve <- function(x, ...) {
  m <- x$getmatrix() #gets the inverse matrix above
  if(!is.null(m)) { ##verify if exists the matrix above
    message("Getting cached data")
    return(m)
  }
  data <- x$get() #if doesnt exists, it takes another matrix
  m <- solve(data, ...) #solves the matrix
  x$setmatrix(m) #set their inverse
  m #display the result
}
