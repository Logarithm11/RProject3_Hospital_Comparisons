##Given a state and one of three outcomes (heart attack, heart failure, and pnemonia),
##returns the state with the lowest 30 day mortality rate for that outcome

best = function(state, outcome){
        data = read.csv("outcome-of-care-measures.csv")
        
        #check for propper input
        if(!(state %in% data$State)) {
                stop("invalid state")
        }
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        
        #sets desired column according to outcome
        column = if(outcome == "heart attack"){
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }
        else if(outcome == "heart failure"){
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }
        else {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        #extracts desired column and rows
        subset = data[data[["State"]] == state, column]
        
        #converts to sortable numeric vector
        subset = as.numeric(levels(subset))[subset]
        
        #sorts from least to greatest 
        subset = sort(subset)
        
        #collects hospitals with lowest mortality rates in alphabetical order
        winners = as.character(sort(data[data[["State"]] == state & data[[column]] == subset[1], "Hospital.Name"]))
        
        winners[1]
}