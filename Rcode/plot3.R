# Assuming:  1) current working directory is C:/user/username
#            2) zip file (of data) is saved in C:/user/username/Desktop/pm25

# Load the data
setwd('./Desktop/pm25')
unzip('exdata-data-NEI_data.zip')
file.remove('exdata-data-NEI_data.zip')
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 3
library(dplyr)
library(ggplot2)
Baltimore <- subset(NEI, NEI$fips == "24510")
Balt_emissionType <- group_by(Baltimore, type, year) %>% summarise(sum(Emissions, na.rm=TRUE))
colnames(Balt_emissionType)[3] = 'Emissions'

png("plot3.png", bg = "transparent")
q <- ggplot(data = Balt_emissionType, aes(x=factor(type), y=Emissions, fill=factor(year))) +
    geom_bar(stat="identity", position = 'dodge')
q + labs(title = 'Trend of Four Baltimore PM2.5 Emission Sources') +
    xlab('Emission Source Types') + theme(plot.title=element_text(size=rel(1.5)))
dev.off()