complete<-function(directory,id=1:332){
  loc<- paste(directory,"/",formatC(id,digits=3,width=0,flag = "0"),".csv",sep = "")
  ##print(loc)
  monitors<-1:length(id)
  ##pol.data<-data.frame()
  comp.obs<-data.frame()
  
  for(i in monitors){
    new.data<-read.csv(loc[i])
    comp.rows<-sum(complete.cases(new.data))
    #print(comp.rows)
    d<-data.frame("id"=id[i],"nobs"=comp.rows)
    #print(d)
    comp.obs<-rbind(comp.obs,d)
    comp.obs
  }
  
  print(comp.obs)
}  

