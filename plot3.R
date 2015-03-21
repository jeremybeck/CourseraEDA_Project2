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
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases 
# in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.


require(dplyr)
require(ggplot2)

# Plot 3 

{
  
  # Read in the formatted data frames provided by the instructors using the readRDS command  
  summary <- readRDS('./exdata-data-NEI_data/summarySCC_PM25.rds')
  mapping <- readRDS('./exdata-data-NEI_data//Source_Classification_Code.rds')
  
  # Get that Baltimore data!
  crabcakecapitaloftheworld <- subset(summary, fips == '24510')
  
  # Group data by year, and source type, then calculate the total emissions for each group. 
  balt_yearly_data <- crabcakecapitaloftheworld %>% group_by(year, type) %>% summarise(yearly_PM25 = sum(Emissions))
  
  # And we define our graphical output, and plot.
  png(file="./plot3.png", height=480, width=1200, units='px')
  
  # We use the facet_grid to break the plot apart, one for each source type. And we'll be a bit lazier here and add a trendline for each facet
  # Fun fact about ggplot2 - if you call within a function or script, you have to use print() to get it to render... 
  
  plot_obj <- ggplot(balt_yearly_data) + geom_point(aes(x=year, y=yearly_PM25, color=type), size=5) + facet_grid(~type) + geom_smooth(method = "lm", se=FALSE, aes(x=year,y=yearly_PM25, color=type)) + 
    labs(title="Trend in Fine Particulate Matter (2.5) Across Source Types - Baltimore (1999-2008)", x="Year",y=expression(Sum~Of~Yearly~PM[2.5]~Readings), colour="Type")
  
  print(plot_obj)
  dev.off()
  
}
