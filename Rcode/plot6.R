# Assuming:  1) current working directory is C:/user/username
#            2) zip file (of data) is saved in C:/user/username/Desktop/pm25

# Load the data
setwd('./Desktop/pm25')
unzip('exdata-data-NEI_data.zip')
file.remove('exdata-data-NEI_data.zip')
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 6
library(dplyr)
library(ggplot2)
motorvehicle <- grep("motor|vehicle|bus|truck|car", SCC$SCC.Level.Three, ignore.case=TRUE)
motorvehicleSCC <- SCC$SCC[motorvehicle]

BC_LA <- subset(NEI, NEI$fips %in% c("24510","06037"))
BC_LA_motorveh <- subset(BC_LA, as.character(BC_LA$SCC) %in% motorvehicleSCC)
BC_LA_motorvehbyYear <- group_by(BC_LA_motorveh, fips, year) %>% summarise(mean(Emissions, na.rm=TRUE))
colnames(BC_LA_motorvehbyYear)[3]  <- 'Emissions'
colnames(BC_LA_motorvehbyYear)[1] <- 'County'
BC_LA_motorvehbyYear$County <- factor(BC_LA_motorvehbyYear$County, levels=c("06037","24510"),
                                      labels=c("Los Angeles", "Baltimore City"))


png("plot6.png", bg="transparent")
ggplot(data = BC_LA_motorvehbyYear, aes(x=County, y=Emissions, color=County)) +
    geom_boxplot() + geom_jitter(size=3) + 
    labs(title = 'Boxplot of PM2.5 Emission from Motor Vehicel: Baltimore vs Los Angeles') +
    xlab('County') + theme(plot.title=element_text(size=rel(0.9)))
dev.off()
