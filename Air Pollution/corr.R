corr<-function(directory,threshold=0){
    loc<-paste(directory,"/",formatC(1:332,digits = 3,width = 0,flag = "0"),".csv",sep="")
    id<-1:332
    monitors<-1:length(id)
    correlation<-numeric()
    for(i in monitors){
      new.data<-read.csv(loc[i])
     # print(loc[i])
      no.comp<-sum(complete.cases(new.data))
      if(no.comp>threshold){
        w<-which(complete.cases(new.data))
        nobs<-sum(complete.cases(new.data))
        #print(nobs)
        comp.new.data<-new.data[w,]
        s.data<-comp.new.data[,"sulfate"]
        n.data<-comp.new.data[,"nitrate"]
        correlation[i]<-cor(s.data,n.data)
        correlation[i]
      }
      else{
        next()
      }
    }
    correlation<-correlation[!is.na(correlation)]
    correlation
}