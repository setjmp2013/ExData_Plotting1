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

## plot3() - Plot a line graphs of the Sub_metering_X fields and
## output it to plot2.png
##
##      data - Dataset to Plot With
plot3 <- function(data) {
        nrows <- nrow(data)
        maxy<-max(
                c(data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3))
        miny<-min(
                c(data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3))
        
        png("plot3.png", width=504, height=504)
        par(bg="transparent")
        plot(
                x=c(0,nrows),y=c(miny,maxy),
                type="n",
                xlab="",
                ylab="Energy sub metering",
                axes=TRUE,
                xaxt="n")
        
        axis(
                1,
                at=c(0,nrows/2,nrows),
                labels=c("Thu","Fri","Sat"))

        with(data, lines(Sub_metering_1, col="black"))
        with(data, lines(Sub_metering_2, col="red"))
        with(data, lines(Sub_metering_3, col="blue"))
        
        legend(
                "topright",
                legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                lty=c(1,1,1),
                lwd=c(1,1,1),
                col=c("black","red","blue"))
        
        dev.off()
}
