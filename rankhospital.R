##Takes three arguments: the 2-character abbreviated name of a
##state (state), an outcome (outcome), and the ranking of a hospital 
##in that state for that outcome (num).The function reads the 
##outcome-of-care-measures.csv file and returns a character vector with 
##the name of the hospital that has the ranking specified by the num argument. 

rankhospital = function(state, outcome, num = "best"){
        data = read.csv("outcome-of-care-measures.csv")
        
        #check for propper input
        if(!(state %in% data$State)) {
                stop("invalid state")
        }
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        if(class(num) == "character" & !(num %in% c("best", "worst"))){
                stop("invalid num")
        }
        
        #sets desired column according to outcome
        column = if(outcome == "heart attack"){
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome == "heart failure"){
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        #extracts desired subset of data frame with removed NAs
        subset = data[data$State == state & data[[column]] != "Not Available", c("Hospital.Name", column)]
        
        #orders subset
        
        if (num == "worst"){
                subset = subset[order(as.numeric(as.character(subset[[column]])), rev(as.character(subset$Hospital.Name)), decreasing = TRUE), ]
        } else{
                subset = subset[order(as.numeric(as.character(subset[[column]])), as.character(subset$Hospital.Name)), ]
        }

        #locates winner
        if (num == "best" | num == "worst"){
                num = 1
        }
        if (num > nrow(subset)){return(NA)}
        
        winner = as.character(subset[num, "Hospital.Name"])
        winner
 }