corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
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
        
        rescor <- numeric()
        
        for (i in 1:332){
                sularray <- numeric()
                nitrarray <- numeric()
                thisnobs <- complete(directory, i)
                if (thisnobs[[2]]>threshold){
                        thisfile <- read.csv(file=get_filepath(i))
                        for (p in (1:nrow(thisfile))){
                                if (!is.na(thisfile[[p,2]]) & !is.na(thisfile[[p,3]])){
                                        sularray <- c(sularray, thisfile[[p,2]])
                                        nitrarray <- c(nitrarray, thisfile[[p,3]])
                                }
                        }
                        rescor <- c(rescor, cor(sularray, nitrarray))
                }
        }
        
        return (rescor)

}
