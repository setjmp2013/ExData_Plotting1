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

## plot2() - Plot a line graph of the  Global_active_power field and
## output it to plot2.png
##
##      data - Dataset to Plot With
plot2 <- function(data) {
        nrows <- nrow(data)

        png("plot2.png",width=504,height=504)
        par(bg="transparent")
        plot(
                data$Global_active_power,
                type="n",
                xlab="",
                ylab="Global Active Power (killowatts)",
                axes=TRUE,
                xaxt="n")
        
        axis(
                1,
                at=c(0,nrows/2,nrows),
                labels=c("Thu","Fri","Sat"))
        
        lines(data$Global_active_power, col="black")
        dev.off()
}
