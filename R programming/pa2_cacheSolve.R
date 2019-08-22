##Assignment: caching the inverse of a matrix
<<<<<<< HEAD
cacheSolve<-function(x,...){ #x is the list returned by makeCacheMatrix
        inver<-x$getInver() #this should be NULL? 
        if(!is.null(inver)){
                message("getting cached matrix")
                return(inver) #if inver is already in the cache, then the function will 
                                # stop here, otherwise, proceed to solve matrix
        }
        data<-x$get() #this will be the matrix
        inver<-solve(data) #assign the solved matrix to inver 
        x$setInver(inver) #set the inver to inver 
        inver #return inver 
        
}
=======
#cacheSolve
#test change
>>>>>>> e8d593c1ecc43b706c2ec7c423f0283dead40bd1
