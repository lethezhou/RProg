rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomech <- c("heart attack", "heart failure", "pneumonia")
        if (outcome=="heart attack") outcol <- 11
        else if (outcome=="heart failure") outcol <- 17
        else if (outcome=="pneumonia") outcol <- 23 #assign outcome to column number
        
        if (is.element(state, outcomedata[,7]) && is.element(outcome, outcomech)){
                #print("go on")
                outcome.state <- subset(outcomedata, State==state) #select state
                outcome.state <- subset(outcome.state, !outcome.state[,outcol]=="Not Available") #select !NA
                order.outcome <- order(as.numeric(outcome.state[,outcol]), outcome.state[,2]) #sort by outcome
                HName <- outcome.state$Hospital.Name[order.outcome] #get sorted Hospital.Name
                if (num =="worst"){
                        return (HName[length(HName)])
                }
                else if (num == "best"){
                        return (HName[1])
                }
                else return (HName[num])
        }
        else if (is.element(outcome, outcomech)){
                stop("invalid state")
        }
        else stop("invalid outcome")        
        
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}