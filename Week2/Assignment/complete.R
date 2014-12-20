complete <- function(directory, id = 1:332) {

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