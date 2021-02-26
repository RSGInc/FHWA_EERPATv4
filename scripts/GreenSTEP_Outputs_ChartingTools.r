#GreenSTEP_Outputs_ChartingTools.r
#=======================

#Copyright 2012, Resource Systems Group, Inc.
#Author: Colin Smith
#Contact: colin.smith@rsginc.om
#Version: 0.1
#Date: 10/29/12
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

#Description
#===========

#This script contains functions to produce various types of charts, which can be called from the other output/reporting scripts

#Load required package to produce charts
#=======================================================
#Packages are loaded in the GreenSTEP.r script

#use ggplot2, reshape
#library(ggplot2)
#library(reshape)
#library(scales)
#library(gsubfn) 

#Code that is run on sourcing to create some basic objects that required to do charting
#======================================================================================

#1. Note: these functions are called with Outputs_ object already loaded

#2. Build metrics data.frame for GreenSTEP Outputs_ file, excluding those that are single values
#TODO: complete formatter, metricname, units
Metrics <- data.frame(t(data.frame(strsplit(names(Outputs_),".",fixed=TRUE),stringsAsFactors = FALSE)),stringsAsFactors = FALSE)
row.names(Metrics) <- names(Outputs_)
names(Metrics) <- c("metric","category")
Metrics <- Metrics[Metrics$metric != Metrics$category,]
Metrics$formatter <- "comma"
Metrics$metricname <- Metrics$metric
Metrics$units <- "(units go here)"

#3. Manage how to aggregate the different tabulations 
#data.frame to organize the different aggregations into a table and include labels for them to name the axes
ucats <- unique(Metrics$category)
ucats.split <- strapply(ucats,"[[:upper:]][[:lower:]]")
ucats.rep <- rep(ucats,lapply(ucats.split,length))
ucats.dim <- unlist(lapply(lapply(ucats.split,length),seq))
ucats.maxd <- rep(unlist(lapply(ucats.split,length)),lapply(ucats.split,length))
ucats.df <- data.frame(category = ucats.rep, aggcode = unlist(ucats.split), dimnum = ucats.dim, maxdimnum = ucats.maxd, stringsAsFactors = FALSE)
aggcodes <- unique(ucats.df$aggcode)
aggregations <- c("County","Income Group","Development Type","Density Group","Mixed Use","Age","Powertrain","Metropolitan Area","Vehicle Type","Fuel Type","Congestion Level","Functional Class","Variables")
aggcodes.df <- data.frame(aggcode = aggcodes, aggregation = aggregations, stringsAsFactors = FALSE)
ucats.df <- merge(ucats.df,aggcodes.df)

#These are the inputs used by the functions
#==========================================

# #the aggregegation type
# #"Co" "Ig" "Dt" "Dg" "Mx" "Ag" "Pt" "Ma" "Ty" "Ft" "Cl" "Fc" "Va"
# aggcode <- "Co"
# 
# #the performance metric selected - needs to match the name of the output tabulation
# #TODO: which metrics are available?
# metrictitle <- "Pop.CoIgDtDg"
# 
# #the type of comparison to perform - comparing by "number", "percentage" or "index"
# measure <- "number"
# measure <- "percentage"
# 
# #make and display the chart
# chart <- makechart(aggcode,metrictitle,measure)
# chart

makechart <- function(aggcode,metrictitle,measure){
  #organize the data and create the chart
  chart <- create1dChart(aggcode,metrictitle,measure)
  #format the chart
  chart <- doChartFormatting(chart,metrictitle,measure)
  
}

create1dChart <- function(aggcode,metrictitle,measure){
  #build the chartdata object by selecting the tabulation to use from Outputs_
  chartdata <- Outputs_[[`metrictitle`]]
  metricname <- Metrics$metricname[row.names(Metrics) == metrictitle]
  unitsname <- Metrics$units[row.names(Metrics) == metrictitle]
  category <- Metrics$category[row.names(Metrics) == metrictitle]
  aggregation <- ucats.df$aggregation[ucats.df$category == category & ucats.df$aggcode == aggcode]
  dimnum <- ucats.df$dimnum[ucats.df$category == category & ucats.df$aggcode == aggcode]
  maxdimnum <- ucats.df$maxdimnum[ucats.df$category == category & ucats.df$aggcode == aggcode]
  
  #summarize the data ready for plotting
  if(maxdimnum > 1){
    chartdata <- margin.table(chartdata,dimnum)
  }
  chartdata <- data.frame(chartdata)
  names(chartdata)[1] <- "value"
  
  #if measure is percentages, convert values to percentages
  if(measure == "percentage"){
      chartdata$value <- prop.table(chartdata$value)
      unitsname <- "(percentage)"
  }
  
  #build the axis names
  yname <- paste(metricname,unitsname,sep=" ")
  xname <- aggregation
  
  #make the chart
  chart <- qplot(row.names(chartdata),data=chartdata, geom="bar", weight = value, main=paste("Comparison of ",metricname," by ", aggregation,sep=""))
  chart <- chart + labs(x=xname,y=yname)
  #remove the legend, uneccessary as the categories are labeled on the x axis
  chart <- chart + theme(legend.position = "none")
  #change the fill color to a more pleasant color that black
  chart <- chart + geom_bar(fill = "#9ECAE1")

  #make sure the last line of code returns the chart else the returned value from the function will be NULL
  chart <- chart
}

doChartFormatting <- function(chart,metrictitle,measure){
  #format y axis so it is numbers with commas separating 1000's
  formatter <- Metrics$formatter[row.names(Metrics) == metrictitle]
  if(measure=="percentage"){formatter <- "percent"}
  if(formatter=="comma"){chart <- chart + scale_y_continuous(labels = comma)}
  if(formatter=="dollar"){chart <- chart + scale_y_continuous(labels = dollar)}
  if(formatter=="percent"){chart <- chart + scale_y_continuous(labels = percent)}
  #font size - make it bigger
  chart <- chart + theme(axis.text.x = element_text(size = 14, angle = 90),
                        axis.text.y = element_text(size = 14),
                        axis.title.x = element_text(size = 16),
                        axis.title.y = element_text(size = 16, angle = 90),
                        legend.text = element_text(size = 14),
                        legend.title = element_text(size = 16),
                        plot.title = element_text(size = 18))
  #modify colors - white background, gray grid lines
  chart <- chart + theme(panel.background = element_rect(fill = NA, colour = "grey80"), 
                        axis.ticks = element_line(colour = "grey80"))
  #make sure the last line of code returns the chart else the returned value from the function will be NULL
  chart <- chart  
}




