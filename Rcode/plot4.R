# Assuming:  1) current working directory is C:/user/username
#            2) zip file (of data) is saved in C:/user/username/Desktop/pm25

# Load the data
setwd('./Desktop/pm25')
unzip('exdata-data-NEI_data.zip')
file.remove('exdata-data-NEI_data.zip')
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 4
library(dplyr)
library(ggplot2)
coalind <- grep("coal", SCC$EI.Sector, ignore.case=TRUE)
coalcombust <- grep('combustion', SCC$SCC.Level.One[coalind], ignore.case=TRUE)
coalcombustSCC <- SCC$SCC[coalcombust]

coalcombustNEI <- subset(NEI, as.character(NEI$SCC) %in% as.character(coalcombustSCC))
coalcombustNEIbyYear <- group_by(coalcombustNEI, year) %>% summarise(sum(Emissions, na.rm=TRUE))
colnames(coalcombustNEIbyYear)[2]  <- 'Emissions'

png("plot4.png", bg="transparent")
ggplot(data=coalcombustNEIbyYear, aes(x=factor(year), y=Emissions)) +
    geom_bar(stat='identity', fill='lightslateblue',colour='slategray',width=0.62) +
    labs(title = 'Trend of PM2.5 Emission from Coal Combustion-related Sources') + 
    xlab('Year') + theme(plot.title=element_text(size=rel(1.2)))
dev.off()