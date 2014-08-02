check_args <- function(state, outcome, data) {
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
    if (!(state %in% data$State))
        stop("invalid state")
}

get_appropriate_column <- function(outcome, data) {
    switch(outcome,
           "heart attack"=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
           "heart failure"=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
           "pneumonia"=data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
}

rankhospital <- function(state, outcome, num="best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    check_args(state, outcome, data)

    data_col <- get_appropriate_column(outcome, data)
    data$target.column <- as.numeric(data_col)
    state_data <- data[data$State == state,]

    ordered_results <- state_data[order(state_data$target.column, state_data$Hospital.Name), ]$Hospital.Name
    if (num == "best")
        ordered_results[[1]]
    else if (num == "worst")
        ordered_results[[length(ordered_results)]]
    else
        ordered_results[[num]]
}
