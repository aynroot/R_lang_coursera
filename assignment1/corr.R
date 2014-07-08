corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    cor_values = numeric()
    for (filename in list.files(directory)) {
        filename <- file.path(directory, filename)
        data <- read.csv(filename)
        cc = complete.cases(data$sulfate, data$nitrate)
        if (length(cc[cc==T]) > threshold) {
            cor_value <- cor(data$sulfate[cc], data$nitrate[cc])
            if (!is.na(cor_value)) {
                cor_values <- c(cor_values, cor_value)
            }
        }
    }
    cor_values
}