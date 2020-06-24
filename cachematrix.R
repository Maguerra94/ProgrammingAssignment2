## Make cache Matrix allows to define a matrix wich can ve converted to its inverse 
## by using he <<- operator which can be used to assign a value to an object in an 
## environment that is different from the current environment, then by get and set
##its stablished the function "inverso" for the inverse of our matrix

makeCacheMatrix <- function(x = matrix()) {
    inverso<-NULL
    set<-function(y){
      x<<-y
      inverso<<-NULL
    }
    get <- function() x
    setInverso <- function(inverso) inverso <<- inverso
    getInverso <- function() inverso
    list(set = set, get = get,
         setInverso = setInverso,
         getInverso = getInverso)
}

## cache solve will verify if the above have returned the inverse otherwise it create 
##a message and then returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverso <- x$getInverso()
  if (!is.null(inverso)) {
    message("getting cached data")
    return(inverso)
  }
  data1 <- x$get()
  inverso <- solve(data1, ...)
  x$setInverso(inverso)
  inverso
}
