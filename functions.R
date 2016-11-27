addTwo <- function(x,y){
  x+y
}

above10<- function(x){
  use<- x>10
  x[use]
}

above<- function(x,y=13){
  use<- x>=y
  x[use]
}

colmean<- function(x,removeNA=T){
  nc<-ncol(x)
  means<-vector("numeric",length = nc)
  for(i in 1:nc){
    means[i]<- mean(x[,i],na.rm = removeNA)
  }
  means
}