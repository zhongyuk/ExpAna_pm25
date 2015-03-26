# Assuming:  1) current working directory is C:/user/username
#            2) zip file (of data) is saved in C:/user/username/Desktop/pm25

# Load the data
setwd('./Desktop/pm25')
unzip('exdata-data-NEI_data.zip')
file.remove('exdata-data-NEI_data.zip')
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 2
library(dplyr)
Baltimore <- subset(NEI, NEI$fips == "24510")
Balt_emission <- group_by(Baltimore, year) %>% summarise(sum(Emissions, na.rm=TRUE))
colnames(Balt_emission)[2] <- 'Baltimore_Emission'

png("plot2.png", bg="transparent")
par(lwd=2)
with(Balt_emission, barplot(Baltimore_Emission, width = 1, space = 0.5,
                            names.arg = Balt_emission$year,
                            col = 'coral', border = 'azure4',
                            main = 'Total PM2.5 Emissions in Baltimore City',
                            xlab = 'Year', ylab = 'Emissions'))
dev.off()