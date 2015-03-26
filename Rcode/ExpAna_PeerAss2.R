
# Load the data
setwd('../')
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
#qplot(x=year,y=Emissions, data=coalcombustNEIbyYear, geom='bar', stat='identity')
ggplot(data=coalcombustNEIbyYear, aes(x=factor(year), y=Emissions)) +
    geom_bar(stat='identity', fill='lightslateblue',colour='slategray',width=0.62) +
    labs(title = 'Trend of PM2.5 Emission from Coal Combustion-related Sources') + 
    xlab('Year') + theme(plot.title=element_text(size=rel(1.2)))
dev.off()


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

BC_LAstat <- group_by(BC_LA_motorvehbyYear, County) %>% summarise(mean(Emissions), sd(Emissions))
colnames(BC_LAstat)[2:3] <- c('avg','std')
BC_LAstat <- transform(BC_LAstat, lower=avg-std,upper=avg+std)


png("plot6.png", bg="transparent")
ggplot(data = BC_LA_motorvehbyYear, aes(x=County, y=Emissions, color=County)) +
    geom_boxplot() + geom_jitter(size=3) + 
    labs(title = 'Boxplot of PM2.5 Emission from Motor Vehicel: Baltimore vs Los Angeles') +
    xlab('County') + theme(plot.title=element_text(size=rel(0.9)))
dev.off()
