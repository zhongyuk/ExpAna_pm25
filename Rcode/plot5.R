# Assuming:  1) current working directory is C:/user/username
#            2) zip file (of data) is saved in C:/user/username/Desktop/pm25

# Load the data
setwd('./Desktop/pm25')
unzip('exdata-data-NEI_data.zip')
file.remove('exdata-data-NEI_data.zip')
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Plot 5
library(dplyr)
library(ggplot2)
Baltimore <- subset(NEI, NEI$fips == "24510")
motorvehicle <- grep("motor|vehicle|bus|truck|car", SCC$SCC.Level.Three, ignore.case=TRUE)
motorvehicleSCC <- SCC$SCC[motorvehicle]
Baltimore_motorvehicel <- subset(Baltimore, as.character(Baltimore$SCC) %in% motorvehicleSCC)
Balt_motorvehicelbyYear <- group_by(Baltimore_motorvehicel,year) %>% summarise(sum(Emissions,na.rm=TRUE))
colnames(Balt_motorvehicelbyYear)[2] <- 'Emissions'

png("plot5.png", bg="transparent")
ggplot(data=Balt_motorvehicelbyYear, aes(x=factor(year), y=Emissions)) +
    geom_bar(stat='identity', fill='yellowgreen',colour='slategray',width=0.62) +
    labs(title = 'Trend of Baltimore PM2.5 Emission from Motor Vehicel Sources') + 
    xlab('Year') + theme(plot.title=element_text(size=rel(1.2)))
dev.off()