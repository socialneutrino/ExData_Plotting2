#Read data from file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
dataBaltimore <- NEI[NEI$fips=="24510",]

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
totalEmissions <- aggregate(Emissions ~ year, data = NEI, sum)
barplot(totalEmissions$Emissions, names=totalEmissions$year)

png(file = "plot1.png", width = 480, height = 480)
title(xlab="year", ylab="Total Emissions", main="Total Emissions by Year")
dev.off()

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
totalBaltimore <- aggregate(Emissions ~ year, data = dataBaltimore, sum)

png(file = "plot2.png", width = 480, height = 480)
barplot(totalBaltimore$Emissions, names=totalBaltimore$year)
title(xlab="year", ylab="Total Emissions", main="Total Emissions by Year")
dev.off()
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
typeEmissions <- aggregate(Emissions ~ year + point, data = dataBaltimore, sum)
typePlot <- ggplot(typeEmissions, aes(year, Emissions, colour=type))
typePlot <- typePlot + geom_line()

png(file = "plot3.png", width = 480, height = 480)
print(typePlot)
dev.off()


#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
coalSCC <- SCC[grep ("Coal", SCC$EI.Sector), 1]
coalNEI <- NEI[NEI$SCC %in% coalSCC,]
totalCoalEmissions <- aggregate(Emissions ~ year, data=coalNEI, sum)

coalPlot <- ggplot(totalCoalEmissions, aes(year, Emissions))
coalPlot <- coalPlot + geom_line()

png(file = "plot4.png", width = 480, height = 480)
print(coalPlot)
dev.off()



#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
motorSCC <- SCC[grep("On-Road", SCC$EI.Sector), 1]
motorBaltimore <- dataBaltimore[dataBaltimore$SCC %in% motorSCC,]
totalBaltMotor <- aggregate(Emissions ~ year, data=motorBaltimore, sum)

vehiclePlot <- ggplot(totalBaltMotor, aes(year, Emissions))
vehiclePlot <- vehiclePlot + geom_line()

png(file = "plot4.png", width = 480, height = 480)
print(vehiclePlot)
dev.off()



#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?



motorSCC <- SCC[grep("On-Road", SCC$EI.Sector), 1]
dataBaltimoreLA <- NEI[NEI$fips=="06037" | NEI$fips=="24510",]
motorBaltimoreLA <- dataBaltimoreLA[dataBaltimoreLA$SCC %in% motorSCC,]
totalBaltimoreLA <- aggregate(Emissions ~ year + fips, data=motorBaltimoreLA, sum)
totalBaltimoreLA$fips <-  ifelse(totalBaltimoreLA$fips=="24510", "Baltimore","LA County")
totalBaltimoreLA$Change <- c(0,diff(totalBaltimoreLA$Emissions[1:4]),0,diff(totalBaltimoreLA$Emissions[5:8]))

motorComparison <- ggplot(plotData, aes(year, Change, color = fips)) + geom_line()

png(file = "plot6.png", width = 480, height = 480)
print(motorComparison)
dev.off()
