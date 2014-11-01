## Caching the inverse of a matrix
## Test with the following after sourcing this file:
## > a<-makeCacheMatrix()
## > a$set(matrix(1:4,2,2))
## > cacheSolve(a)
## and then once more:
## > cacheSolve(a)
## to get the cached result.
## Create a special "matrix" object that can cache its inverse

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


## Compute inverse of matrix returned by makeCacheMatrix, check
## if inverse already available


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
