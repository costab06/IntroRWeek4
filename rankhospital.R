rankhospital <- function(state, outcome, num = "best") {
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
                
                if (num == "best" || num == "worst" || is.numeric(num)) {
                        
                        if (outcome %in% valid.outcomes) {
                                
                                if (outcome == "heart attack") {
                                        
                                        ## reduce the df to the necessary columns
                                        reducedOutcomeDF <- 
                                                outcomeDF[outcomeDF$State == state,
                                                          c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.Name")]
                                        
                                        ## convert the character content to numeric
                                        reducedOutcomeDF[, 1] <- as.numeric(reducedOutcomeDF[, 1])
                                        
                                        
                                        ## sort by the occurances
                                        reducedOutcomeDF <- 
                                                reducedOutcomeDF[with(reducedOutcomeDF,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                                                                             Hospital.Name)), ]
                                        ## remove the "Not Available"
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[complete.cases(reducedOutcomeDF),]
                                        
                                        
                                } else if (outcome == "heart failure") {
                                        
                                        reducedOutcomeDF <- 
                                                outcomeDF[outcomeDF$State == state,
                                                          c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.Name")]

                                        ## convert the character content to numeric
                                        reducedOutcomeDF[, 1] <- as.numeric(reducedOutcomeDF[, 1])
                                        
                                        
                                        
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[with(reducedOutcomeDF,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                                                                                             Hospital.Name)), ]
                                        ## remove the "Not Available"
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[complete.cases(reducedOutcomeDF),]
                                        
                                } else if (outcome == "pneumonia") {
                                        
                                        reducedOutcomeDF <- 
                                                outcomeDF[outcomeDF$State == state,
                                                          c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia","Hospital.Name")]

                                        
                                        ## convert the character content to numeric
                                        reducedOutcomeDF[, 1] <- as.numeric(reducedOutcomeDF[, 1])
                                        
                                        
                                        
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[with(reducedOutcomeDF,order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, 
                                                                                             Hospital.Name)), ]
                                        ## remove the "Not Available"
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[complete.cases(reducedOutcomeDF),]
                
                                        
                                }
                                
                                
                                ## handle best and worst
                                if (num == "best") {
                                        num <- 1
                                } else if (num == "worst") {
                                        num <- nrow(reducedOutcomeDF)
                                }
                                
                                if (num > nrow(reducedOutcomeDF)) {
                                        hospitalName = NA
                                } else {
                                        
                                        
                                        
                                        ## Return hospital name in that state with the given rank
                                        ## 30-day death rate
                                        print(reducedOutcomeDF)
                                        
                                        hospitalName <- reducedOutcomeDF[num,"Hospital.Name"]
                                }
                                
                        } else {
                                stop("invalid outcome")
                        }
                } else {
                        stop("invalid num")
                }
        } else {
                stop ("invalid state")
        }
        
        hospitalName
}