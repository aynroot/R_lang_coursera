check_args <- function(outcome, data) {
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
}

get_appropriate_column <- function(outcome, data) {
    switch(outcome,
           "heart attack"=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
           "heart failure"=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
           "pneumonia"=data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
}

rank_one_state <- function(state_data, num) {
    state_data <- state_data[, c("target.column", "Hospital.Name", "State")]
    indices <- do.call(order, state_data)

    if (num == "best")
        num <- 1
    else
        if (num == "worst")
            num <- max(indices)
    c(hospital = state_data[indices, ][num, "Hospital.Name"], state = state_data[1, "State"])
}

rankall <- function(outcome, num="best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    check_args(outcome, data)

    data_col <- get_appropriate_column(outcome, data)
    data$target.column <- as.numeric(data_col)

    states_data <- split(data[, c("target.column", "Hospital.Name", "State")], data$State)
    as.data.frame(do.call(rbind, lapply(states_data, rank_one_state, num)))
}