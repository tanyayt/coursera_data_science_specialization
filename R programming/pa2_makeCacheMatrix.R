##Assignment: caching the inverse of a matrix
#makeCacheMatrix creates a special "matrix" object that cache its inverse
makeCacheMatrix<-function(x=matrix()){ #initiate M as an matrix
        inver<-NULL #initiate inver as as NULL object within makeCacheMatrix()
        set<-function(y){
                x<<-y #set a given value to x that's defined in makeCacheMatrix
                        #the "<<-" assigns the value y to an object x in the parent environment
                inver<<-NULL #reset inver to NULL
        } 
        get<-function(){x} #returns the matrix x
        setInver <-function(z){
                inver<<-z
        }#set a function z to inver at the parent level
        getInver<-function(){inver}
        list(set=set,get=get,setInver=setInver,getInver=getInver)#returing a new list object by giving all the functions defined within, to the parent envir
}

