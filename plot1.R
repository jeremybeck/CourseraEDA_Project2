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
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


require(dplyr)
require(ggplot2)

## Plot 1 
{
  
  # Read in the formatted data frames provided by the instructors using the readRDS command  
  summary <- readRDS('./exdata-data-NEI_data/summarySCC_PM25.rds')
  mapping <- readRDS('./exdata-data-NEI_data//Source_Classification_Code.rds')
  
  # Use dplyr to group the data by year, and calculate the total emissions within each year. 
  yearly_data <- summary %>% group_by(year) %>% summarise(yearly_PM25 = sum(Emissions))
  
  # Now just to help with visualizating the trend we'll fit a linear model to the 4 points
  # Note this is simply for plotting purposes.  We aren't evaluating quality of fit,
  trndlm <- lm(yearly_PM25 ~ year, data=yearly_data)
  
  
  # And we define our graphical output, and plot.
  png(file="./plot1.png", height=480, width=480, units='px')
  
  # Plot the data - I really hate the expression() syntax, but other options for subscripts were throwing errors. 
  plot(yearly_data$year, yearly_data$yearly_PM25, pch=16, col="black", xlab="Year", ylab=expression(Sum~Of~Yearly~PM[2.5]~Readings), main=expression(Yearly~Trend~In~Total~PM[2.5]~(1999~-~2008)))
  # Add the trend line
  lines(yearly_data$year, predict(trndlm), col="red", lty=2, lwd=2)
  
  dev.off()
  
  
}
