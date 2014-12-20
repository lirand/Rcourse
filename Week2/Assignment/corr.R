corr <- function(directory, threshold = 0) {
        cr <- c()
        cc <- complete(directory)
        cc_thresh <- cc[cc$nobs > threshold, ]
        for (i in cc_thresh$id) {
                file_name <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
                data <- read.csv(file_name)
                cr <- c(cr, (cor(data$sulfate, data$nitrate,  use="pairwise.complete.obs" )))
        
        }
        return(cr)
}



