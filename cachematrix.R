## makeCacheMatrix store the cache value if the respective operation is done atleast once on same data and 
##cacheSolve will check for cacheData if available else caluclates the mean


## This funciton is used to cache the data if already caluclated to avoid further computations
makeCacheMatrix <- function(x = matrix()) {
     ## To store the result
      inv<-NULL
      set<-function(y=matrix()){ ##used to define variables in different environment
        x<<-y
        inv<<-NULL
      }
      ## get is assigned an anonymous function which returns x
      get<-function() x
      setinverse<-function(solve) inv<<-solve
      getinverse<-function() inv
      ##returns a list of functions
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
      }


## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result<-x$getinverse()
        if(!is.null(result)) ## Check if there is cache Data available
        {
          message("cache data")
          return (result) ##return result 
        }
        ##if cache data has no result then it caluclates the inverse
        givenMatrix <- x$get()
        inv_of_x <- solve(givenMatrix,...)
        ## update cache data with this value so that same operation, if done again, can be avoided
        x$setinverse(inv_of_x)
        inv_of_x
        
        
  
}
