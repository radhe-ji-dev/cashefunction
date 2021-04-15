makeCachmatrix<-function(x=matrix()){
  int<-NULL
  set<-function(y){
    x<<-y
    int<<-NULL
  }
  
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function() {inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  }

#function to invert the matrix

cachesolve<-function(x,...){
  inv<-x$getInverse()
  
  if(!is.null(inv))
    {
    message("gettting cached data")
    return(inv)
    }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}
  