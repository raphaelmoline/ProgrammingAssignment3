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
    
    print("to be completed")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}