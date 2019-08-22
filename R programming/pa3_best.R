best<-function(state,outcome){
        ## set up three empty dataframes
        data<-data.frame() #data stores the full data set
        state_data<-data.frame()#state_data stores the subset for the selected state
        state_outcome_data<-data.frame() #state_outcome_data stores the subset based on state and outcome
        #read full data
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        #check if state is valid
        if(!is.element(state,data$state)){
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
        ## use filter() and select()?

        #coerce the column to numeric based on the outcome
        if (outcome=="heart attack"){
                data[,11]<-as.numeric(data[,11])
        }
        else if (outcome=="heart failure"){
               data[,17]<-as.numeric(data[,17])
        }
        else if (outcome=="pneumonia"){
                data[,23]<-as.numeric(data[,23])
        }

        #find the lowest value in outcome data and returns the hospital index



        ##return hospital name in the state with lowest 30-day death rate

}