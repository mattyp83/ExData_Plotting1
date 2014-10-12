plot3 <- function(filetxt = "household_power_consumption.txt"){

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
days$Sub_metering_1 <- as.numeric(as.character(days$Sub_metering_1))
days$Sub_metering_2 <- as.numeric(as.character(days$Sub_metering_2))
days$Sub_metering_3 <- as.numeric(as.character(days$Sub_metering_3))
png(filename = "plot3.png")
plot(days$Sub_metering_1, type = "l", ylim=c(0,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
par(new = T)
plot(days$Sub_metering_3, type = "l", ylim=c(0,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "blue")
par(new = T)
plot(days$Sub_metering_2, type = "l", ylim=c(0,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "Energy sub metering", col = "red")
axis(1, at = c(0, 1440, 2880), labels= c("Thursday", "Friday", "Saturday"))
axis(2, at = c(0, 10, 20, 30), labels = c("0", "10", "20", "30"))
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty=1)
dev.off()
}