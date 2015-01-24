## makeCacheMatrix stores a matrix and "cache" its inverse
## cachesolve computes the inverse of the matrix stored in makeCacheMatrix and it first
## checks if the inverse has been computed earlier. If the inverse exists it skips the computation
## if the inverse has not been computed it calculates it and stores it in cache using setsolve function
## to use the functions, if A is a matrix, run : cachesolve(makeCacheMatrix(A))

## makeCacheMatrix stores a matrix and "cache" its inverse

makeCacheMatrix <- function(x = matrix()) {
  
      m<-NULL
      set<-function(y) {
            x<<-y
            m<<-NULL
      }

  get<-function()x
  setsolve<-function(solve) m<<-solve
  getsolve<-function()m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
      
}


## cachesolve computes the inverse of the matrix stored in makeCacheMatrix and it first
## checks if the inverse has been computed earlier. If the inverse exists it skips the computation
## if the inverse has not been computed it calculates it and stores it in cache 
## using setsolve function

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      m<-x$getsolve()
      if(!is.null(m)){
        message("getting cached data")
        return(m)
      }
      data<-x$get()
      m<-solve(data)
      x$setsolve(m)
      m


}
