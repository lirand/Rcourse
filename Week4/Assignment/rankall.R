rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        if(outcome == "heart attack") {
                col <- 11
        } else if(outcome == "heart failure") {
                col <- 17
        } else if(outcome == "pneumonia") {
                col <- 23
        } else {
                stop("invalid outcome")
        }
        
        ## determine num
        
        if (num == "best") {
                num <- 1
        }
        
        new_num <- NULL
        a <- NULL
        b <- NULL

        
        states <- unique(data[order(data[, 7] , na.last = NA),7])
        
        for(i in states) {
                new_num <- num
                state_data <- data[i == data[, 7],]
                state_data[, col] <- as.numeric(state_data[, col])
                order_result <- state_data[order(state_data[, col], state_data$Hospital.Name, na.last = NA), ]
        
                if (num == "worst") {
                        new_num <- length(order_result[,2])
                }
                
                a <- c(a, order_result[new_num,2])
                b <- c(b, i)
        
        }
        
        result <- data.frame(hospital = a, state = b, b, row.names = 3)
}
