# Script to complete plot for the Second Course Project of the 
# Exploratory Data Analysis Course on Coursera
# Data were obtained from the following URL:
#
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#
# Just a friendly reminder to everyone that this is EDA, and the instructors
# point out many times in lectures that these plots are meant to be illustrative,
# but not neccessarily have polished and perfectly formatted axes.  
# Let's have fun, and thanks for evaluating!


# This script addresses the following question:
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


require(dplyr)
require(ggplot2)

# Plot 5
{
  
  # Read in the formatted data frames provided by the instructors using the readRDS command
  summary <- readRDS('./exdata-data-NEI_data/summarySCC_PM25.rds')
  mapping <- readRDS('./exdata-data-NEI_data//Source_Classification_Code.rds')
  
  # The Mapping Data shows that any motor vehicles will start with "Mobile - On-Road" in the "EI.Sector" column.
  # We aren't going to consider lawn-mowers as motor vehicles, even though my neighbor seems to think they are.  
  # We also aren't going to consider airplanes or boats as motor vehicles for the sake of this exercise. 
  mobile_scc <- mapping[grepl("Mobile - On-Road", mapping$EI.Sector),"SCC"]
  
  # And now we just subset the dataframe using the FIPS for Baltimore, and the Mobile SCCs list. 
  crabcakecapitaloftheworld_cars <- subset(summary, fips == '24510' & SCC %in% mobile_scc)
  
  
  # Let's group the data by year in dplyr, taking the yearly PM2.5 to be the sum of 'Emissions'
  balt_yearly_cardata <- crabcakecapitaloftheworld_cars %>% group_by(year) %>% summarise(yearly_PM25 = sum(Emissions))
  
  # And we define our graphical output, and plot. 
  png(file="./plot5.png", height=480, width=480, units='px')
  
  # A pretty standard qplot call. 
  # Fun fact about ggplot2 - if you call within a function or script, you have to use print() to get it to render...
  
  plot_obj <- qplot(year, yearly_PM25, data=balt_yearly_cardata, xlab="Year", ylim=c(0,350), ylab=expression(Sum~Of~Yearly~PM[2.5]~Readings), 
        main=expression(Yearly~Trend~In~Total~PM[2.5]~From~Car~Emissions~In~Baltimore))
  
  print(plot_obj)
  dev.off()
  
}
