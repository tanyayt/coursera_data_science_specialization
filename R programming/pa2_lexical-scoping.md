pa2\_lexical scoping
================

# About

Course: R Programming Platform: Coursera Assignment: Programming
Assignment 2: Lexical Scoping

# Goal

Wrte an R function to cache potentially time-consuming computations.

# Knowledge

``` 
    * <<- operator to assign a value to an object in an environment that's differnt from the current environment 
    
```

# makeCacheMatrix

This function creates a special matrix object that can cache its inverse

``` r
makeCacheMatrix <-function (x=matrix()) {
        #set initial value x to an empty matrix
        m <-NULL 
        set <- function (y){
                x <<-y
                m <<-NULL
        }
        
        get <- function ()x # assign value x to get function
        setinverse <-function(solve) {m<<-solve} # assign solve to m 
        getinverse <-function() m
        
        list (setvalue = set, getvalue= get,
              setinverse_value = setinverse,
              getinverse_value = getinverse)
}
```

\#cacheSolve

This function calculates the inverse of the matrix x; if the inverse is
in the cache, the calculation is not executed rather just give the value
in cache

``` r
cacheSolve <-function(x){
        m <-x$getinverse()
        if(!is.null(m)){
                message("cached data already existed")
                m
        }
        
        data<-x$get()
        m<- solve(data)
        x$setinverse(m)
        m

}
```
