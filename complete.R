complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        get_filepath <- function(number){
                if (number < 10){
                        return (paste(directory, "/", "00", number, ".csv", sep=""))
                }
                else if (number < 100){
                        return (paste(directory, "/", "0", number, ".csv", sep=""))
                        
                }
                else{
                        return (paste(directory, "/", number, ".csv", sep=""))
                }
        }
        
        nobs <- integer()
        
        for (i in id){
                thisfile <- read.csv(file=get_filepath(i))
                thisnobs <- 0
                for (p in (1:nrow(thisfile))){
                        if (!is.na(thisfile[[p,2]]) & !is.na(thisfile[[p,3]])){
                                thisnobs <- thisnobs+1
                        }
                }
                nobs <- c(nobs, thisnobs)
                
        }
        return (data.frame(id, nobs))














}