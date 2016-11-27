pollutantmean<- function(directory,pollutant,id=1:332){
      loc<- paste(directory,"/",formatC(id,digits = 3,width=0,flag = "0"),".csv",sep = "")
      ##print(loc)
      monitors<-1:length(id)
      pol.data<-data.frame()
      for(i in monitors){
        new.data<-read.csv(loc[i])
        print(loc[i])
        pol.data<-rbind(pol.data,new.data)
      }
      
      pollutant.values<-pol.data[,pollutant]
      pol.mean<-mean(pollutant.values,na.rm = TRUE)
      pol.mean
}