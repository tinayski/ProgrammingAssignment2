## The functions in this program will cache the inverse of a matrix
## 

## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 i<-NULL #set inverse initially to NULL
 
 set<-function(y){
   x<<-y
   i<<-NULL
 }
 
 get<-function(){x} #get matrix x
 setInv<-function(inv){ i<<-inv}
 getInv<-function(){i} #get inverse of matrix
 
 list(set=set, get=get, setInv=setInv, getInv=getInv)
}


##computes the invere of a special matrix. if the inverse is already calculated
## then it will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  i<-x$getInv() 
  
  if(!is.null(i)){ #check if there is inverse data in cache
    message("Getting cached data")
    return(i)
  }
  m<-x$get()
  i<-solve(m,...) #calculates inverse
  x$setInv(i)
  i #return inverse
}
