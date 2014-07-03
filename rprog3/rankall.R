rankhospital <- function(data, outcol, num = "best") {
        ## Read outcome data
        outcomedata <- data

        #outcome.state <- subset(outcomedata, State==state) #select state
        order.outcome <- order(as.numeric(outcomedata[,outcol]), outcomedata[,2]) #sort by outcome
        HName <- outcomedata$Hospital.Name[order.outcome] #get sorted Hospital.Name
        
                if (num =="worst"){
                        return (HName[length(HName)])
                }
                else if (num == "best"){
                        return (HName[1])
                }
                else return (HName[num])
}
              


rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomech <- c("heart attack", "heart failure", "pneumonia")
        if (outcome=="heart attack") outcol <- 11
        else if (outcome=="heart failure") outcol <- 17
        else if (outcome=="pneumonia") outcol <- 23 #assign outcome to column number
        
        if (is.element(outcome, outcomech)){
                #print("go on")
                outcome.order <- order(outcomedata$State)
                outcomedata <- outcomedata[outcome.order,] #sort by State
                outcomedata <- subset(outcomedata, !outcomedata[,outcol]=="Not Available")
                outcomedate.splited <- split(outcomedata, outcomedata$State) #splite into States
                
                hospital <- character()
                state <- character()
                for (i in 1:54){
                        hospital <- c(hospital, rankhospital(outcomedate.splited[[i]], outcol, num))
                        state <- c(state, outcomedate.splited[[i]]$State[[1]])
                }
                
                #print (state.name)
                output <- data.frame(hospital, state)
                rownames(output) <- output$state
                return(output)
                
        }
        else stop("invalid outcome")
        
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}