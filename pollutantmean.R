pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  i <- 0
  allpollution <- numeric()
  get_filepath <- function(number){
          if (number < 10){
                  return (paste("specdata/", "00", number, ".csv", sep=""))
          }
          else if (number < 100){
                  return (paste("specdata/", "0", number, ".csv", sep=""))
                  
          }
          else{
                  return (paste("specdata/", number, ".csv", sep=""))
          }
  }
  
  for (i in id){
          thispoint <- read.csv(file=get_filepath(i))
          if (pollutant == "sulfate"){
                  allpollution <- c(allpollution, thispoint[[2]])
          }
          else{
                  allpollution <- c(allpollution, thispoint[[3]])
          }
          
  }
  
  return (round(mean(allpollution, na.rm = TRUE),3))
  
  
}