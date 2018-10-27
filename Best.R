best <- function(state, outcome) {
    ## Read outcome data
    dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state is valid by counting the records matching that state name
    if (sum(dataset[,7] == state) == 0) {
        stop("invalid state")
    }
    if (!(outcome  %in% c("pneumonia", "heart attack", "heart failure")) ) {
        stop("invalid outcome")
    }
    
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
        lowest <- min(extract[,3])
        results <- extract[extract[,3] == lowest,1]
        results <-sort(results)
    }
    
    #heart failure case
    if (outcome == "heart failure") {
        extract <- extract[complete.cases(extract[,4]),]
        lowest <- min(extract[,4])
        results <- extract[extract[,4] == lowest,1]
        results <-sort(results)
    }
    
    #pneumonia case
    if (outcome == "pneumonia") {
        extract <- extract[complete.cases(extract[,5]),]
        lowest <- min(extract[,5])
        results <- extract[extract[,5] == lowest,1]
        results <-sort(results)
    }

    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    results[1]
}