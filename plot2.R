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
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question


require(dplyr)
require(ggplot2)

# Plot 2 
{
  
  # Read in the formatted data frames provided by the instructors using the readRDS command  
  summary <- readRDS('./exdata-data-NEI_data/summarySCC_PM25.rds')
  mapping <- readRDS('./exdata-data-NEI_data//Source_Classification_Code.rds')
  
  # Get that blue crab data for Baltimore!
  crabcakecapitaloftheworld <- subset(summary, fips == '24510')
  
  # Summarize the data using dplyr to group by year and calculate total emissions within each year
  balt_yearly_data <- crabcakecapitaloftheworld %>% group_by(year) %>% summarise(yearly_PM25 = sum(Emissions))
  
  # Now just to help with visualizating the trend we'll fit a linear model to the 4 points
  # Note this is simply for plotting purposes.  We aren't evaluating quality of fit,
  trndlm <- lm(yearly_PM25 ~ year, data=balt_yearly_data)
  
  # And we define our graphical output, and plot.
  png(file="./plot2.png", height=480, width=480, units='px')
  # Plot the data
  plot(balt_yearly_data$year, balt_yearly_data$yearly_PM25, pch=16, col="black", xlab="Year", ylab=expression(Sum~Of~Yearly~PM[2.5]~Readings), 
       main=expression(Yearly~Trend~In~Total~PM[2.5]~For~Baltimore~(1999~-~2008)))
  # Add the trendline
  lines(balt_yearly_data$year, predict(trndlm), col="red", lty=2, lwd=2)
  
  dev.off()
  
}
