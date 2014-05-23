## 'makeCacheMatrix' is a function that makes a list of other functions for inputting a matrix, viewing/passing
## an input matrix, setting the matrix in a cache, and viewing/passing the matrix in a cache.
## 'cacheSolve is a function that takes a matrix and either returns a previously cached inverse, or computes the 
## inverse, returns it, and caches it for future use.


## makeCacheMatrix:
## 'set' is a function (in the makeCacheMatrix list) which takes the user's input (y) and sets the variable x
## equal to it in the makeCacheMatrix environment. It also sets the varaible "MatCache" equal to NULL in the 
## makeCacheMatrix environment.

## 'get' is a function that returns the matrix passed to the function by makeCacheMatrix or set

## 'setMatCache' is a function that takes an inverse matrix (returned by cacheSolve) and sets MatCache (the cache)
## equal to it.

## 'getMatCache is a function that returns the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  MatCache <- NULL
  set <- function(y) {
    x <<- y
    MatCache <<- NULL
  }
  get <- function() x
  setMatCache <- function(inverse) MatCache <<- inverse
  getMatCache <- function() MatCache
  list(set = set, get = get,
       setMatCache = setMatCache,
       getMatCache = getMatCache)
}


## 'cacheSolve is a function that takes a matrix and either returns a previously cached inverse, or computes the 
## inverse, returns it, and caches it for future use.

cacheSolve <- function(x, ...) {
  cache <- x$getMatCache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data,...)
  x$setMatCache(cache)
  cache
}
