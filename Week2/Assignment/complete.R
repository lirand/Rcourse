complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicatng the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        result <- c()
        for(i in 1:length(id)) {
                ## add zeros for file normalization
                file_len <- nchar(id[i])
                if (file_len == 1) {
                        file_name <- paste("00", id[i], sep = '')
                } else if (file_len == 2) {
                        file_name <- paste("0", id[i], sep = '')
                } else {
                        file_name <- id[i]
                }
                # create full file name
                file_name_ext <- paste(file_name, ".csv", sep = '')
                
                # create full path 
                data <- read.csv(file.path(directory, file_name_ext))
                result[i]  <- sum(complete.cases(data))
        }
        data.frame(cbind(id, nobs=result))
}