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
  sum <- c()
  for(i in id) {
          ## add zeros for file normalization
          file_len <- nchar(i)
                if (file_len == 1) {
                        file_name <- paste("00", i, sep = '')
                } else if (file_len == 2) {
                        file_name <- paste("0", i, sep = '')
                }
          # create full file name
          file_name_ext <- paste(file_name, ".csv", sep = '')
          
          # create full path 
          file <- read.csv(file.path(directory, file_name_ext))
          
          # remove NA and read proper column
          na_list <- is.na(file[,pollutant])
          sum <- c(sum, file[!na_list,pollutant])
          
  }
  mean(round(sum,3))
  
}