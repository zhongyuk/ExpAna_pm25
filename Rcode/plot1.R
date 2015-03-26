# Assuming:  1) current working directory is C:/user/username
#            2) zip file (of data) is saved in C:/user/username/Desktop/pm25

# Load the data
setwd('./Desktop/pm25')
unzip('exdata-data-NEI_data.zip')
file.remove('exdata-data-NEI_data.zip')
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 1
library(dplyr)
total_emission <- group_by(NEI, year) %>% summarise(sum(Emissions, na.rm=TRUE))
colnames(total_emission)[2] <- 'total_emissions'

png("plot1.png", bg='transparent')
par(lwd = 2)
with(total_emission, barplot(total_emissions, width = 1, space = 0.5,
                             names.arg = total_emission$year,
                             col = 'cadetblue1', border = 'azure4',
                             main = 'Total PM2.5 U.S. Emissions',
                             xlab = 'Year', ylab = 'Emissions'))
dev.off()

