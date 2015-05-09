## Explore the data set and a single graphic with 4 individual plots placed on a page
## Assumptions:
## 1. the data file is in the current working directory
## 2. the data file has a header with the column names
## 3. missing values are represented by "?" character
## 4. dates in column 1 are chr in the format dd/mm/yyyy
## 5. time in column 2 are chr in the format hh:mm:sss
## 6. the other columns are numeric
plot4 <- function() {
      ## check we have a data file
      data.file <- paste(getwd(),"/household_power_consumption.txt", sep = "")
      if(!file.exists(data.file)) {
            print(paste("Data file", data.file, "does not exist in the current directory so cannot be loaded."))
            stop(paste("Please load the file in the working directory", getwd()), call. = FALSE)
      }
      ## read the data file now we know it exists
      ## get only the lines that we need (1st/2nd Feb 2007) from the file and manually set column headings
      ## now we have an existing file so read the data
      data <- read.table(data.file, header = TRUE, sep = ";", na.strings = "?", stringsAsFactors=FALSE )
      ## the dates we want to subset
      good.dates <- as.Date(c('01/02/2007', '02/02/2007'), format = "%d/%m/%Y")
      ## get a list of dates from the data to be used to subset
      all.dates <-  as.Date(data$Date, format = "%d/%m/%Y")
      plot.data <- data[all.dates %in% good.dates, ] ## extract the 2 days data we want
      ##
      ## get our datetime vector 
      datetime <- strptime(paste(plot.data[, 1], plot.data[, 2]), format = "%d/%m/%Y %H:%M:%S")
      plot.data <- cbind(plot.data, as.data.frame(datetime))
      ## set up the frame for the 4 plots, column-wise 2x2
      par(mfcol = c(2,2))
      ## and plot the data - Plot 1 - Global Active Power against date/time
      plot(plot.data$datetime, plot.data$Global_active_power, col="black", type="l",
           xlab = NA, ylab = "Global Active Power" )
      ## and plot the data - Plot 2 - 3 vars for Sub Metering against date/time
      with(plot.data, {
            plot(plot.data$datetime, plot.data$Sub_metering_1, xlab = NA, 
                 ylab = "Energy sub metering", col="black", type="l")
            ## Plot the additional variables we want to see
            lines(plot.data$datetime, plot.data$Sub_metering_2, col="red")
            lines(plot.data$datetime, plot.data$Sub_metering_3, col="blue")
            ## and ensure we have a legend
            legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, 
                   col=c("black","red","blue"), bty="n")
            }
      )
      ## and plot the data - Plot 3 - Voltage against date/time
      with(plot.data,plot(plot.data$datetime, plot.data$Voltage, col="black", type="l", 
                          xlab = "datetime", ylab = "Voltage" ))  
      ## and plot the data - Plot 4 - Global Reactive Power against date/time
      with(plot.data, plot(plot.data$datetime, plot.data$Global_reactive_power, col="black", type="l",
                           xlab = "datetime", ylab = "Global_reactive_power" ))  
      ## Done

} # end of function



