best <- function(state, outcome) {
  ## Read outcome data
  hosp.data<-read.csv("hospital data/outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  states<-unique(hosp.data$State)
  outcomes<-c("heart attack","heart failure","pneumonia")
  
  if(is.element(state,states)){
    if(is.element(outcome,outcomes)){
      ##subsetting state data
      states.data<-split(hosp.data,hosp.data$State)
      state.hosp.data<-states.data[state]
      state.hosp.data<-data.frame(state.hosp.data)
    
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      index<-match(outcome,outcomes)
      hospital<-list()
      if(index==1) i=11
      if(index==2) i=17
      if(index==3) i=23
      suppressWarnings(state.hosp.data[,i]<-as.numeric(state.hosp.data[,i]))
      w<-complete.cases(state.hosp.data)
      state.hosp.data<-state.hosp.data[w,]
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      w.best<-which.min(state.hosp.data[,i])
      print(state.hosp.data[w.best,2])
      
    }
    else{
      stop("invalid Outcome")
    }
  }
  else{
    stop("invalid State")
  }
  
  
}