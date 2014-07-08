pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    levels = numeric(length=0)
    for (id_name in id) {
        filename = sprintf("%03d", id_name)
        filename = file.path(directory, paste(filename, "csv", sep="."))
        data = read.csv(filename)
        levels <- c(levels, data[[pollutant]])
    }
    mean(levels, na.rm=T)
}