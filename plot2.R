#Read data from file
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset NEI data frame to just include Baltimore emissions
dataBaltimore <- NEI[NEI$fips=="24510",]

#Aggregate Baltimore emission data, summing all emissions by year
totalBaltimore <- aggregate(Emissions ~ year, data = dataBaltimore, sum)

#Create barplot total emissions against year and save to png file
png(file = "plot2.png", width = 480, height = 480)
barplot(totalBaltimore$Emissions, names=totalBaltimore$year)
title(xlab="year", ylab="Total Emissions (tons)", 
	main="Total Emissions in Baltimore by Year")
dev.off()
