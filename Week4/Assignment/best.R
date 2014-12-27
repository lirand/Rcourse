best <- function(state, outcome) {
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

        if(!is.element(state, data[,7])) {
                stop("invalid state")
        }

        ## Return hospital name in that state with lowest 30-day death
        ## rate

        state_data <- data[state == data[, 7],]
        state_data[, col] <- as.numeric(state_data[, col])
        order_result <- state_data[order(state_data[, col], state_data$Hospital.Name), ]
        order_result[1,2]
        
}
