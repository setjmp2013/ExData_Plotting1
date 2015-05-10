
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

## plot1() - Plot a line graph of the  Global_active_power field and
## output it to plot2.png
##
##      data - Dataset to Plot With
plot1 <- function(data) {
        nrows <- nrow(data)
        
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
}

## plot2() - Plot a line graphs of the Sub_metering_X fields and
## output it to plot2.png
##
##      data - Dataset to Plot With
plot2 <- function(data) {
        nrows <- nrow(data)
        maxy<-max(
                c(data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3))
        miny<-min(
                c(data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3))
        
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
        
}

## plot3() - 
##
plot3 <- function(data) {
        plot(data$Voltage,type="n",xaxt="n",xlab="datetime",ylab="Voltage")
}

## plot4() -
##
plot4 <- function(data) {
        plot(data$Voltage,type="n",xaxt="n",xlab="datetime",ylab="Global_reactive_power")
}
## plotAll() - Create and do all the graphs.
plotAll <- function(data) {
        par(mfcol = c(2,2),bg="transparent")
        plot1(data)
        plot2(data)
        plot3(data)
        plot4(data)
        
        dev.copy(png, "plot4.png")
        dev.off()
}