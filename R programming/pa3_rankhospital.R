#programming assignment 3: ranking hospital
#state is the two character abbrv name, outcome is the outcome
#num is the ranking
rankhospital<-function(state,outcome,num){
        #validate outcome
        if(!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
                message("invalid outcome")
                return(NA)

        }
        #validate num
        if(!(num=="best"|num=="worst"|class(as.numeric(num))=="numeric")){
                message("invalid rank")
                return(NA)
        }
        chosen_state<-state #rename chosen_state to avoid confusion
        outcome<-tolower(outcome)#make outcome lower case
        num<-tolower(num)
        #reads csv file
        data<-data.frame()
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        data[,11]<-as.numeric(data[,11])
        data[,17]<-as.numeric(data[,17])
        data[,23]<-as.numeric(data[,23])
        #validate state
        if(!is.element(chosen_state,data$State)){
                message("invalid state")
                return(NA)
        }
        #create state_data subset
        state_data<-filter(data,State==chosen_state)
        #rank state_data based on outcome, state_outcome_data stores hospital names, from best to worst
        state_outcome_data<-data.frame()
        if(outcome=="heart attack"){
                state_outcome_data<-arrange(state_data,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name) #sorted
                state_outcome_data<-na.omit(state_outcome_data[,2])
        }
        else if(outcome=="heart failure"){
                state_outcome_data<-arrange(state_data,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)
                state_outcome_data<-na.omit(state_outcome_data[,2])
        }
        else if(outcome=="pneumonia"){
                state_outcome_data<-arrange(state_data,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)
                state_outcome_data<-na.omit(state_outcome_data[,2])
        }
        #next evaluate num to see which row to return
        if(num=="best"){
                return(state_outcome_data[1])
        }
        else if(num=="worst"){
                return(tail(state_outcome_data,1))
        }
        else if(class(as.numeric(num))=="numeric"){
                num<-round(as.numeric(num)) #convert to a rounded value
                if(num>length(state_outcome_data)){
                       return(NA)
                }
                else return(state_outcome_data[as.numeric(num)])
        }

}