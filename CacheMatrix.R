library (MASS)
make cachematrix  <- function(x = matrix()) { 
inv <- NULL
set <- function(y) {
                 x<<-y
                 inv<<-NULL
                   }
 get<-function()x     
 setinv<-function(inverse)inv<<-inverse
 getinv<-function() {
                  inver<-ginv(x)
                  inver%%x
                    }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
 }
 
 
 cachesolve <-function(x, ...)
    {
    inv<-$getinv()
    if(!is.null(inv))  {
       message("getting cache data")
       return(inv)
       }
     data <-x$get()
     inv<-solve(data, ...)
     x$setinv(inv)
     inv
     }

o/p: ##
       f<-makeCacheMatrix(matrix(1:8, 2,4))
       f$get()
       f$getinv()
       cachesolve(f)
       
