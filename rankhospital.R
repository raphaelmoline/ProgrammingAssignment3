rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (sum(dataset[,7] == state) == 0) {
        stop("invalid state")
    }
    if (!(outcome  %in% c("pneumonia", "heart attack", "heart failure")) ) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    # extract the subset of data for state and mortality rates
    extract <- dataset[dataset[,7] == state,c(2,7,11,17,25)]
    
    #rename columns
    colnames(extract)[3] <- "Attack"
    colnames(extract)[4] <- "Failure"
    colnames(extract)[5] <- "Pneumonia"
    
    #coerce as numeric
    extract[,3] <- as.numeric(extract[,3])
    extract[,4] <- as.numeric(extract[,4])
    extract[,5] <- as.numeric(extract[,5])
    
    #heart attack case
    if (outcome == "heart attack") {
        extract <- extract[complete.cases(extract[,3]),]
        extract <- extract[order(extract[,3],extract[,1]),]
    }
    
    #heart failure case
    if (outcome == "heart failure") {
        extract <- extract[complete.cases(extract[,4]),]
        extract <- extract[order(extract[,4],extract[,1]),]
    }
    
    #heart pneumonia case
    if (outcome == "pneumonia") {
        extract <- extract[complete.cases(extract[,5]),]
        extract <- extract[order(extract[,5],extract[,1]),]
    }
    
    if (num == "best") {num <- 1}
    if (num == "worst") {num <- dim(extract)[1]}
    extract[num,1]
    ## 30-day death rate
}