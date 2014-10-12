plot2 <- function(filetxt = "household_power_consumption.txt"){
    ##download text file to WD first
    #name of data file is assigned to filetxt
    #filetxt <- "household_power_consumption.txt"
    #read in data to to data object
    data <- read.csv(filetxt, header = T, sep = ";")
    #convert Date column from factor to character
    data$Date <- strptime(as.character(data$Date), format = "%d/%m/%Y")
    #convert Date column from character to Date format
    data$Date <- as.Date(data$Date)
    #subset data from 2007-02-01
    day1 <- data[data$Date == "2007-02-01",]
    #subset data from 2007-02-02
    day2 <- data[data$Date == "2007-02-02",]
    #combine the two subsets
    days <- rbind(day1, day2)
    #convert global_active_power from factor to character
    days$Global_active_power <- as.character(days$Global_active_power)
    #convert global_active_power from character to numeric
    days$Global_active_power <- as.numeric(days$Global_active_power)
    #plot histogram of global_active_power with red columns, and required labels & title
    png(filename = "plot2.png")
    plot(days$Global_active_power, type = "l", xaxt = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
    axis(1, at = c(0, 1440, 2880), labels= c("Thursday", "Friday", "Saturday"))
   # dev.copy(png, file = "plot2.png")
    dev.off()
}