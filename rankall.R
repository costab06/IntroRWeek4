rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        
        ## create reference data for the states and outcomes
        valid.states <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL",
                          "IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH",
                          "NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VI","VT",
                          "WA","WI","WV","WY")
        
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        
        if (num == "best" || num == "worst" || is.numeric(num)) {
                
                if (outcome %in% valid.outcomes) {
                        
                        
                        hospitalNames <- character()
                        
                        
                        ## loop taking the state out of the valid.states vector and run this
                        for (state in valid.states) {
                                
                                
                                if (outcome == "heart attack") {
                                        
                                        
                                        
                                        ## reduce the df to the necessary columns
                                        reducedOutcomeDF <- 
                                                outcomeDF[outcomeDF$State == state,
                                                          c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.Name", "State")]
                                        
                                        ## convert the character content to numeric
                                        reducedOutcomeDF[, 1] <- as.numeric(reducedOutcomeDF[, 1])
                                        
                                        
                                        ## sort by the occurances
                                        reducedOutcomeDF <- 
                                                reducedOutcomeDF[with(reducedOutcomeDF,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                                                                             Hospital.Name)), ]
                                        ## remove the "Not Available"
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[complete.cases(reducedOutcomeDF),]
                                        
                                        if (num == "best") {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[1,"Hospital.Name"])
                                        } else if (num == "worst") {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[nrow(reducedOutcomeDF),"Hospital.Name"])
                                                
                                        } else {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[num,"Hospital.Name"]) 
                                                
                                        }
                                        
                                        
                                        
                                } else if (outcome == "heart failure") {
                                        
                                        reducedOutcomeDF <- 
                                                outcomeDF[outcomeDF$State == state,
                                                          c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.Name","State")]
                                        
                                        ## convert the character content to numeric
                                        reducedOutcomeDF[, 1] <- as.numeric(reducedOutcomeDF[, 1])
                                        
                                        
                                        
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[with(reducedOutcomeDF,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                                                                                             Hospital.Name)), ]
                                        ## remove the "Not Available"
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[complete.cases(reducedOutcomeDF),]
                                        
                                        
                                        if (num == "best") {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[1,"Hospital.Name"])
                                        } else if (num == "worst") {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[nrow(reducedOutcomeDF),"Hospital.Name"])
                                                
                                        } else {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[num,"Hospital.Name"]) 
                                                
                                        }
                                        
                                        
                                } else if (outcome == "pneumonia") {
                                        
                                        reducedOutcomeDF <- 
                                                outcomeDF[outcomeDF$State == state,
                                                          c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia","Hospital.Name","State")]
                                        
                                        
                                        ## convert the character content to numeric
                                        reducedOutcomeDF[, 1] <- as.numeric(reducedOutcomeDF[, 1])
                                        
                                        
                                        
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[with(reducedOutcomeDF,order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, 
                                                                                             Hospital.Name)), ]
                                        ## remove the "Not Available"
                                        reducedOutcomeDF <-
                                                reducedOutcomeDF[complete.cases(reducedOutcomeDF),]
                                        
                                        ## handle best and worst
                                        if (num == "best") {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[1,"Hospital.Name"])
                                        } else if (num == "worst") {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[nrow(reducedOutcomeDF),"Hospital.Name"])
                                               
                                        } else {
                                                hospitalNames <- c(hospitalNames, reducedOutcomeDF[num,"Hospital.Name"]) 
                                                
                                        }
                                
                                        
                                }
                                
                        } ## for
                        
                        
                        ## Return a data frame with the hospital names and the
                        ## (abbreviated) state name
                        df <- data.frame(hospitalNames)
                        df <- cbind(df, valid.states)
                        colnames(df) <- c("hospital", "state")
                        rownames(df) <- valid.states
                } else {
                        stop("invalid outcome")
                }
        } else {
                stop("invalid num")
        }
        
        df
}
