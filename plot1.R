## getCleanDataset() - Get the dataset from the file and strip it down to
## the required date range only.
getCleanDataset <- function() {
        data <-read.csv(
                "household_power_consumption.txt",
                sep=";",
                colClasses=c("character",
                             "character",
                             "numeric",
                             "numeric",
                             "numeric",
                             "numeric",
                             "numeric",
                             "numeric",
                             "numeric"),
                stringsAsFactors=FALSE,
                na.strings="?",
                blank.lines.skip=TRUE)
        
        data$Date <- strptime(
                paste(data$Date,data$Time, sep=" "),
                format="%d/%m/%Y %H:%M:%S")
        
        data <- subset(
                data,
                Date >="2007-02-01 00:00:00" & Date <= "2007-02-02 23:59:59",
                select=c(1,3,4,5,6,7,8,9))
        
        row.names(data)<-NULL
        
        data
}

## plot1() - Plot a neat histogram of the Global_active_power field and
## output it to plot1.png
##
##      data - Dataset to Plot With
plot1 <- function(data) {
        png("plot1.png",width=504, height=504)
        par(bg="transparent")
        hist(
                data$Global_active_power,
                main="Global Active Power",
                xlab="Global Active Power (kilowatts)",
                col="red")
        
        dev.off()
}
