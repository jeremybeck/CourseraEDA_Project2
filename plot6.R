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
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?


require(dplyr)
require(ggplot2)

# Plot 6
{
  
  # Read in the formatted data frames provided by the instructors using the readRDS command
  summary <- readRDS('./exdata-data-NEI_data/summarySCC_PM25.rds')
  mapping <- readRDS('./exdata-data-NEI_data//Source_Classification_Code.rds')
  
  # The Mapping Data shows that any motor vehicles will start with "Mobile - On-Road" in the "EI.Sector" column.
  # We aren't going to consider lawn-mowers as motor vehicles, even though my neighbor seems to think they are.  
  mobile_scc <- mapping[grepl("Mobile - On-Road", mapping$EI.Sector),"SCC"]
  
  # Now let's filter based on whether the FIPS code corresponds to either Baltimore or LA County, 
  # AND the SCC corresponds to an entry in our 'mobile' list. 
  baltorla_cars <- subset(summary, (fips == '24510' | fips == '06037') & SCC %in% mobile_scc)
  
  # Use dplyr to group by year, and then FIPS, calculate the total PM25 for each year
  # Then group by location, sort in chronological order of years, and calculate the emissions as a percent change from the earliest value 
  # in each group of the dataframe, using the first() option in dplyr. 
  # I am using the values relative to 1999 here since the question asks to show how much the emissions change,
  # and I think this is more meaningful as a percent change since populations, traffic, etc are very different at either location. 
  baltorla_yearly_cardata <- baltorla_cars %>% group_by(year,fips) %>% summarise(yearly_PM25 = sum(Emissions)) %>% group_by(fips) %>% 
    arrange(year) %>% mutate(Emissions_rel = 100*(yearly_PM25-first(yearly_PM25))/first(yearly_PM25))
  
  # Now let's just replace the FIPS with meaningful labels for the plots
  baltorla_yearly_cardata$fips[baltorla_yearly_cardata$fips == "06037"] <- paste("LA_County")
  baltorla_yearly_cardata$fips[baltorla_yearly_cardata$fips == "24510"] <- paste("Balt_City")
  
  
  # And we define our graphical output, and plot. 
  png(file="./plot6.png", height=480, width=720, units='px')
  
  # This ggplot call is split out as two separate plots, one for each location, using the facet_grid() option.
  # A trendline is also added using the geom_smooth() options with a linear model. 
  # Fun fact about ggplot2 - if you call within a function or script, you have to use print() to get it to render...
  
  plot_obj <- ggplot(baltorla_yearly_cardata) + geom_point(aes(x=year, y=Emissions_rel, color=fips), size=5) + facet_grid(~fips) + 
    geom_smooth(method = "lm", se=F, formula = y ~ x, aes(x=year,y=Emissions_rel, color=fips)) + 
    labs(title="Percent Change in PM2.5 Relative to 1999 Levels - Baltimore vs LA (1999-2008)", x="Year",
         y=expression(Percent~Change~In~PM[2.5]~Relative~To~1999~Levels), colour="fips") + theme(legend.position="none")
  
  print(plot_obj)
  dev.off()
  
}
