best <- function(state, outcome) {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomech <- c("heart attack", "heart failure", "pneumonia")
        if (outcome=="heart attack") outcol <- 11
        else if (outcome=="heart failure") outcol <- 17
        else if (outcome=="pneumonia") outcol <- 23
        if (is.element(state, outcomedata[,7]) && is.element(outcome, outcomech)){
                #print("go on")
                outcome.state <- subset(outcomedata, State==state)
                order.outcome <- order(as.numeric(outcome.state[,outcol]), outcome.state[,2])
                HName <- outcome.state$Hospital.Name[order.outcome]
                return (HName[1])
        }
        else if (is.element(outcome, outcomech)){
                stop("invalid state")
        }
        else stop("invalid outcome")        
        
        
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}