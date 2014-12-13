pollutantmean <- function(directory, pollutant, id = 1:332) {
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