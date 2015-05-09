## Explore the data set and build a histogram of the frequency of Global active power measures
## Assumptions:
## 1. the data file is in the current working directory
## 2. the data file has a header with the column names
## 3. missing values are represented by "?" character
## 4. dates in column 1 are chr in the format dd/mm/yyyy
## 5. time in column 2 are chr in the format hh:mm:ss
## 6. the other columns are numeric
plot1 <- function() {
      ## check we have a data file
data.file <- paste(getwd(),"/household_power_consumption.txt", sep = "")
if(!file.exists(data.file)) {
      print(paste("Data file", data.file, "does not exist in the current directory so cannot be loaded."))
      stop(paste("Please load the file in the working directory", getwd()), call. = FALSE)
}
      ## now we have an existing file so read the data
data <- read.table(data.file, header = TRUE, sep = ";", na.strings = "?", stringsAsFactors=FALSE )
      ## the dates we want to subset
good.dates <- as.Date(c('01/02/2007', '02/02/2007'), format = "%d/%m/%Y")
      ## get a list of dates from the data to be used to subset
all.dates <-  as.Date(data$Date, format = "%d/%m/%Y")
plot.data <- data[all.dates %in% good.dates, ] ## extract the 2 days data we want
      ## now plot the histogram - set breaks to 12 to replicate the given plot
      ## and have it plot the frequency
hist(plot.data$Global_active_power, col="red", freq=TRUE, breaks = 12, 
     xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power" )
} # end of function



      