rankall = function(outcome, num = "best"){
        ##reads outcome data
        data = read.csv("outcome-of-care-measures.csv")
        
        ##checks for valid state and outcome
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
        
        ##finds hospital for given rank for each state
        
        #empty starter columns
        hospital = character()
        state = character()
        
        #extracts desired subset
        for(st in levels(data$State)){
                
                #subsets by state, excluding NA's
                subset = data[data$State == st & data[[column]] != "Not Available", c("Hospital.Name", column)]

                #orders by outcome then hospital name
                subset$Hospital.Name = as.character(subset$Hospital.Name)
                subset[[column]] = as.numeric(as.character(subset[[column]]))
                
                if (num == "worst"){
                        subset = subset[order(subset[[column]], rev(subset$Hospital.Name), decreasing = TRUE), ]
                } else{
                        subset = subset[order(subset[[column]], subset$Hospital.Name), ]
                }
                
                #finds desired value
                index = num
                if (num == "best" | num == "worst"){
                        index = 1
                }
                if (index > nrow(subset)){
                        value = NA
                }else{
                        value = as.character(subset[index, "Hospital.Name"])
                }
                
                #concatenates to data frame columns
                hospital = c(hospital, value)
                state = c(state, st)
        }
        
        #returns data frame with hospital names and state
        #abriviations
        data.frame(hospital = hospital, state = state)
}