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
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?


require(dplyr)
require(ggplot2)

# Plot 4 - NEED TO VERIFY DATA AND FINISH PLOT
{
  
  # Read in the formatted data frames provided by the instructors using the readRDS command
  summary <- readRDS('./exdata-data-NEI_data/summarySCC_PM25.rds')
  mapping <- readRDS('./exdata-data-NEI_data//Source_Classification_Code.rds')  
  
  # Looking at SCC Mapping Tables, we can see in the "EI.Sector" Column that Any Row with Coal Combustion starts with "Fuel Combustion"
  # and ends with "- Coal".  We will therefore search for both "Combustion" and "Coal" in that field, and take only those rows' SCC value. 
  coal_scc <- mapping[grepl("Combustion", mapping$SCC.Level.One) & grepl("Coal", mapping$SCC.Level.Three),"SCC"]
  
  # Now we just filter the SummarySCC Data against those SCC Codes.
  summary_coalcomb <- subset(summary, SCC %in% coal_scc)
  
  # Let's now aggregate the summary data by year, taking the total PM2.5 in each year as the sum of emissions in each group.
  coalcomb_yearly_data <- summary_coalcomb %>% group_by(year) %>% summarise(yearly_PM25 = sum(Emissions))
  
  
  # And we define our graphical output, and plot. 
  png(file="./plot4.png", height=480, width=480, units='px')
  
  # Pretty normal qplot call here. 
  # Fun fact about ggplot2 - if you call within a function or script, you have to use print() to get it to render...
  
  plot_obj <- qplot(year, yearly_PM25, data=coalcomb_yearly_data, xlab="Year", ylab=expression(Sum~Of~Yearly~PM[2.5]~Readings), 
        main=expression(Yearly~Trend~In~Total~PM[2.5]~From~Coal~Combustion))
  
  print(plot_obj)
  
  dev.off()
  
}
