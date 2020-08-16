library(tidyr)
library(dplyr)

url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
if(!dir.exists("Data")) dir.create("Data")
download.file(url, destfile = "./Data/Electric Power Consumtion.zip")
unzip("./Data/Electric Power Consumtion.zip", exdir = "./Data")

data = read.table("./Data/household_power_consumption.txt",
                  sep = ";",
                  header = T,
                  na.strings = "?"
                  ) %>%
    mutate(Time = paste(Date, Time) %>% strptime("%d/%m/%Y %H:%M:%S"),
           Date = as.Date(Date, "%d/%m/%Y")
    ) %>%
    (function(X) {
        X[, 3:9] = sapply(X[, 3:9], as.numeric)
        return(X)
    }) %>%
    filter(Date %in% as.Date(0:1, "2007-02-01"))

# Plot 1
png("./plot1.png", width = 480, height = 480, units = "px")
hist(data$Global_active_power,
     main = "Global active Power", xlab = "Global active Power (kilowatts)",
     col = "red")
dev.off()

# Plot 2
png("./plot2.png", width = 480, height = 480, units = "px")
plot(data$Time, data$Global_active_power,
     main = "Global active Power", xlab = "Global active Power (kilowatts)",
     type = "l")
dev.off()

# Plot 3
png("./plot3.png", width = 480, height = 480, units = "px")
plot(quantile(data$Time), quantile(data$Sub_metering_1), 
     main = "Global Active Power", xlab = "Date", ylab = "Energy sub Metering",
     "n")
lines(data$Time, data$Sub_metering_1, col = "black")
lines(data$Time, data$Sub_metering_2, col = "red")
lines(data$Time, data$Sub_metering_3, col = "blue")
legend(x = "topright",
       legend = paste("Metering", 1:3), 
       col = c("black", "red", "blue"),
       lty = 1, bty = "n")
dev.off()

# Plot 4
png("./plot4.png", width = 480, height = 480, units = "px")
par(mfrow = c(2, 2))
plot(data$Time, data$Global_active_power,
     xlab = "Global active Power (kilowatts)",
     type = "l")
title("Global Active Power")

plot(data$Time, data$Voltage,
     xlab = "Voltage",
     type = "l")

plot(quantile(data$Time), quantile(data$Sub_metering_1),
     xlab = "Date", ylab = "Energy sub Metering",
     "n")
lines(data$Time, data$Sub_metering_1, col = "black")
lines(data$Time, data$Sub_metering_2, col = "red")
lines(data$Time, data$Sub_metering_3, col = "blue")
legend(x = "topright",
       legend = paste("Metering", 1:3), 
       col = c("black", "red", "blue"),
       lty = 1, bty = "n")

plot(data$Time, data$Global_reactive_power,
     xlab = "Global Reactive Power",
     type = "l")
dev.off()
