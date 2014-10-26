## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y)
        {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(invpassed)inv<<-invpassed
        getinv<-function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv))
        {
         message("getting cached value")
         return (inv)
        }
        data<-x$get()
        x$setinv(solve(data))
        ## Return a matrix that is the inverse of 'x'
        return (solve(data))
}
