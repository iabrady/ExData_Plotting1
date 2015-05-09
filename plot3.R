## Explore the data set and build a plot showing 3 sub metering vars against date/time 
## Assumptions:
## 1. the data file is in the current working directory
## 2. the data file has a header with the column names
## 3. missing values are represented by "?" character
## 4. dates in column 1 are chr in the format dd/mm/yyyy
## 5. time in column 2 are chr in the format hh:mm:ss
## 6. the other columns are numeric
plot3 <- function() {
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
      ##
      ## get our datetime vector 
      Date_Time <- strptime(paste(plot.data[, 1], plot.data[, 2]), format = "%d/%m/%Y %H:%M:%S")
      plot.data <- cbind(plot.data, as.data.frame(Date_Time))
      ## and plot the data with the first parameter as the first line and set titles etc
      with(plot.data, plot(plot.data$Date_Time, plot.data$Sub_metering_1, xlab = NA, 
                           ylab = "Energy sub metering", col="black", type="l"))
      ## Plot the additional variables we want to see
      lines(plot.data$Date_Time, plot.data$Sub_metering_2, col="red")
      lines(plot.data$Date_Time, plot.data$Sub_metering_3, col="blue")
      ## and ensure we have a legend
      legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black","red","blue"))
  
} # end of function



