rankall<-function(outcome,num = "best"){
  ## Read outcome data and split it into a list a/c to states
  hosp.data<-read.csv("hospital data/outcome-of-care-measures.csv",colClasses = "character")
  states<-unique(hosp.data$State)
  states.data<-split(hosp.data,hosp.data$State)
  
  ##initialise the result data frame
  ranked.data<-data.frame()
  
  ##checking outcome
  outcomes<-c("heart attack","heart failure","pneumonia")
  if(is.element(outcome,outcomes)){
    ##optimising the daat frames/elements of the list states.data
    index<-match(outcome,outcomes)
    if(index==1) i=11
    if(index==2) i=17
    if(index==3) i=23
    suppressWarnings(states.data<-lapply(states.data,function(elem){
      elem[,i]<-as.numeric(elem[,i])
      elem<-elem[complete.cases(elem),]
      arrange<-order(elem[,i],elem[,2])
      elem<-elem[arrange,]
      #ranked.data<-rbind(ranked.data,new.frame)
    }))
    
    states.data<-do.call("rbind",states.data)
    states.data<-states.data[,c(2,7)]
    states.data<-split(states.data,states.data$State)
    
    lapply(states.data,function(elem){
      if(num =="best") rank<-1
      if(num=="worst") rank<-nrow(elem)
      if(is.numeric(num)) rank<-num
      
      new.frame<-elem[rank,]
      #nrow(elem)
      ranked.data<-rbind.data.frame(ranked.data,new.frame)
      
    })
      
  }  
  else{
    stop("invalid outcome")
  }
}