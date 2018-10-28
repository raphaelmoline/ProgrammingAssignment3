rankall <- function(outcome, num = "best") {
    ## Read outcome data
    dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(outcome  %in% c("pneumonia", "heart attack", "heart failure")) ) {
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    # extract the subset of data for state and mortality rates
    extract <- dataset[,c(2,7,11,17,23)]
    
    #note: code refactoring to be considered and use an variable value depending on outcome to avoid code repetition (i=3/4/5)
    
    #rename columns
    colnames(extract)[3] <- "Attack"
    colnames(extract)[4] <- "Failure"
    colnames(extract)[5] <- "Pneumonia"
    
    #coerce as numeric
    extract[,3] <- as.numeric(extract[,3])
    extract[,4] <- as.numeric(extract[,4])
    extract[,5] <- as.numeric(extract[,5])
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    #heart attack case
    if (outcome == "heart attack") {
        extract <- extract[complete.cases(extract[,3]),]
        extract <- extract[,c(1,2,3)]
    }
    
    #heart failure case
    if (outcome == "heart failure") {
        extract <- extract[complete.cases(extract[,4]),]
        extract <- extract[,c(1,2,4)]
    }
    
    #heart pneumonia case
    if (outcome == "pneumonia") {
        extract <- extract[complete.cases(extract[,5]),]
        extract <- extract[,c(1,2,5)]
    }
    
    results <- data.frame()
    
    for (i in unique(extract$State)) {
        tmp <- extract[extract[,2] == i,]
        tmp <- tmp[order(tmp[,3],tmp[,1]),]
        print(tmp)
        num2 <- num
        if (num == "best") {num2 <- 1}
        if (num == "worst") {num2 <- dim(tmp)[1]}
        record <- data.frame(tmp[num2,1],i)
        #print(record)
        results <- rbind(results,record)

    }
    
    colnames(results) <- c("hospital", "state")
    results[,2] <- as.character(results[,2]) # these are considered factors and cannot be sorted properly
    results <- results[order(results$state),]
    results
}