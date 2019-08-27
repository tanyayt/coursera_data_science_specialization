rankall<-function(outcome,num="best"){#default for num is best
        #read outcome data
        data<-data.frame()
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        data[,11]<-as.numeric(data[,11])
        data[,17]<-as.numeric(data[,17])
        data[,23]<-as.numeric(data[,23])
        #check outcome is valid
        outcome<-tolower(outcome)#make outcome lower case
        if(!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
                message("invalid outcome")
                return(NA)

        }
        #check num is valid
        num<-tolower(num)
        if(!(num=="best"|num=="worst")){
                num<-as.numeric(num)
                if(is.na(num)){
                        message("invalid rank,not a number or best or worst")
                        return(NA)
                }
        }


        #create subgroup based on outcome
        library(dplyr)
        outcome_data<-data.frame()
        nth_outcome_data<-data.frame()
        if (outcome=="heart attack"){
                outcome_data<-select(data,hospital=Hospital.Name,state=State,outcome=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                #clean for complete cases, rank
                outcome_data<-outcome_data %>% filter(complete.cases(.))%>%
                arrange(state,outcome,hospital)%>%
                group_by(state)%>%
                mutate(rank_in_state=rank(outcome,ties.method = "first"))


        }
        else if(outcome=="heart failure"){
                outcome_data<-select(data,hospital=Hospital.Name,state=State,outcome="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
                #clean for complete cases, rank
                outcome_data<-outcome_data %>% filter(complete.cases(.))%>%
                arrange(state,outcome,hospital)%>%
                group_by(state)%>%
                mutate(rank_in_state=rank(outcome,ties.method = "first"))
        }
        else if(outcome=="pneumonia"){
                outcome_data<-select(data,hospital=Hospital.Name,state=State,outcome="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
                #clean for complete cases, rank
                outcome_data<-outcome_data %>% filter(complete.cases(.))%>%
                arrange(state,outcome,hospital)%>%
                group_by(state)%>%
                mutate(rank_in_state=rank(outcome,ties.method = "first"))

        }
        else{
                message("invalid outcome")
                return(NA)
        }
        #outcome_data now stores the outcome data grouped by State and ordered within the state
        #return a data frame with the hospital name
        #and the abbreviated state name based on num
        #when num == best or numeric
        if (num=="best"){
                num<-1
        }
        # once best is converted to 1
        if(is.numeric(num)){
                num<-round(as.numeric(num))
                if(num<=0){
                        return(NA)
                }
                ## creating the data frame
                nth_outcome_data<-outcome_data
                nth_outcome_data<-filter(outcome_data,rank_in_state==num)
                return(nth_outcome_data[,1:2])
        }
        else if(num=="worst"){
                ##creating the data frame, desc, and return the first index
                nth_outcome_data<-outcome_data
                nth_outcome_data<-nth_outcome_data%>%
                arrange(state,desc(rank_in_state))%>%
                group_by(state)%>%
                summarise(hospital=first(hospital))
                return(nth_outcome_data[,1:2])
        }
        ##if no return values up to this point
        else{
                message("invalid num")
                return(NA)
        }

}