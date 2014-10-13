plot4 <- function(filetxt = "household_power_consumption.txt"){
    
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
    #######################################################################
    #Global Active Power
    #convert global_active_power from factor to character
    days$Global_active_power <- as.character(days$Global_active_power)
    #convert global_active_power from character to numeric
    days$Global_active_power <- as.numeric(days$Global_active_power)
    #plot histogram of global_active_power with red columns, and required         labels & title
    png(filename = "plot4.png")
    par(mfrow = c(2,2))
    plot(days$Global_active_power, type = "l", xaxt = "n", xlab = "", ylab = "Global Active Power")
    axis(1, at = c(0, 1440, 2880), labels= c("Thu", "Fri", "Sat"))
    #######################################################################
    #######################################################################
    #Voltage
    days$Voltage <- as.numeric(as.character(days$Voltage))
    plot(days$Voltage, type= "l", xaxt = "n", xlab = "datetime", ylab = "Voltage")
    axis(1, at = c(0, 1440, 2880), labels= c("Thu", "Fri", "Sat"))
    #######################################################################
    #######################################################################
    #Sub Metering
    days$Sub_metering_1 <- as.numeric(as.character(days$Sub_metering_1))
    days$Sub_metering_2 <- as.numeric(as.character(days$Sub_metering_2))
    days$Sub_metering_3 <- as.numeric(as.character(days$Sub_metering_3))
    plot(days$Sub_metering_1, type = "l", ylim=c(0,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    par(new = T)
    plot(days$Sub_metering_3, type = "l", ylim=c(0,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "blue")
    par(new = T)
    plot(days$Sub_metering_2, type = "l", ylim=c(0,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "Energy sub metering", col = "red")
    axis(1, at = c(0, 1440, 2880), labels= c("Thu", "Fri", "Sat"))
    axis(2, at = c(0, 10, 20, 30), labels = c("0", "10", "20", "30"))
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty=1,bty = "n")
    #######################################################################
    days$Global_reactive_power <- as.numeric(as.character(days$Global_reactive_power))
    plot(days$Global_reactive_power, type = "l", xaxt = "n", xlab="datetime", ylab = "Global_reactive_power")
    axis(1, at = c(0, 1440, 2880), labels= c("Thu", "Fri", "Sat"))
dev.off()    
    
}