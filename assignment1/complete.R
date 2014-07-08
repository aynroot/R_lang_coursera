complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    nobs = numeric()
    for (id_name in id) {
        filename = sprintf("%03d", id_name)
        filename = file.path(directory, paste(filename, "csv", sep="."))
        data = read.csv(filename)
        cc <- complete.cases(data)
        nobs <- c(nobs, length(cc[cc==T]))
    }
    data.frame(id, nobs)
}