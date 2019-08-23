best<-function(state,outcome){
        ## set up three empty dataframes
        data<-data.frame() #data stores the full data set
        state_data<-data.frame()#state_data stores the subset for the selected state
        state_outcome_data<-data.frame() #state_outcome_data stores the subset based on state and outcome
        chosen_state<-state #assign the state argument to chosen state for readiability
        outcome<-tolower(outcome)#convert outcome to lower case to tolerate error
        #read full data
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        data[,11]<-as.numeric(data[,11])
        data[,17]<-as.numeric(data[,17])
        data[,23]<-as.numeric(data[,23])
        #check if state is valid
        if(!is.element(chosen_state,data$State)){
                message("invalid state")
                return(NA)
        }
        #check if outcome is valid
        if(!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
                message("invalid outcome")
                return(NA)

        }
        #using dplyr package (uncomment the next two line to install if needed)
        ##install.packages("dplyr")
        ##library(dplr)
        ## use filter() to choose only the rows of chosen state
        state_data<-filter(data,State==chosen_state)
        ##arrange by the outcome measure and then return the first row
        if(outcome=="heart attack"){
                state_outcome_data<-arrange(state_data,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name) #sorted
                return(state_outcome_data[1,2])
        }
        else if(outcome=="heart failure"){
                state_outcome_data<-arrange(state_data,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)
                return(state_outcome_data[1,2])
        }

        else if(outcome=="pneumonia"){
                state_outcome_data<-arrange(state_data,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)
                return(state_outcome_data[1,2])
        }
}