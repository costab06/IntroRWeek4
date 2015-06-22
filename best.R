best <- function(state, outcome) {
        ## Read outcome data
        outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        
        
        ## create reference data for the states and outcomes
        valid.states <- c("AK","AL","AR","AZ","CA","CO","CT","DE","FL","GA","HI","IA","ID","IL",
                          "IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH",
                          "NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT",
                          "WA","WI","WV","WY")
        
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        
        ## Check that state and outcome are valid
        if (state %in% valid.states) {
                
                
                if (outcome %in% valid.outcomes) {
                        
                        if (outcome == "heart attack") {
                                colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                                reducedOutcomeDF <- outcomeDF[outcomeDF$State == state,c(colName,"Hospital.Name")]
                                fullColName <- reducedOutcomeDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                        } else if (outcome == "heart failure") {
                                colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                                reducedOutcomeDF <- outcomeDF[outcomeDF$State == state,c(colName,"Hospital.Name")]
                                fullColName <- reducedOutcomeDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                        } else if (outcome == "pneumonia") {
                                colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                                reducedOutcomeDF <- outcomeDF[outcomeDF$State == state,c(colName,"Hospital.Name")]
                                fullColName <- reducedOutcomeDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                        }
                        
                        ## get the complete cases                        
                        complete <- complete.cases(reducedOutcomeDF)
                        reducedOutcomeDF[complete,]
                        
                        ## Get the hospital name for the min from the reducedOutcomesDF
                        hospitalName <- reducedOutcomeDF[which.min(fullColName),"Hospital.Name"]
                        
                        
                        
                } else {
                        stop("invalid outcome")
                }
        } else {
                stop ("invalid state")
        }
        
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        hospitalName
        
}



