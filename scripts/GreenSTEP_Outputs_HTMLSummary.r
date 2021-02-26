#GreenSTEP_Outputs_HTMLSummary.r
#===============================

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

#This script produces some high level summaries from data in Outputs_ and writes them to an HTML file for each RunYear

#Define function to format tables
#=======================================================

#Function to help build formatted tables
formattable <- function(columnnames, rownames, thedata, totalrow = FALSE){
  #build the table
  thetable <- data.frame(thedata)
  names(thetable) <- columnnames
  #add a total row if needed
  if(totalrow){
    thetotals <-  sapply(thedata,sum)
    thetable <- rbind(thetable,thetotals)
    rownames <- c(rownames,"Total")
  }
  #add rownames
  rownames(thetable) <- rownames
  #format the data
  thetable <- format(round(thetable,0), justify = "right", big.mark = ",")
}

# to avoid problems with referening the right graphics device
graphics.off()

#Produce the output for the current forecasy year
#=========================

#for( yr in RunYears ) {
  
  print( paste("Writing Year ", yr, " Output Report") )
  RunYearDir <- paste( "outputs/Year", yr, sep="" )
  
  #Load the outputs file
  #=====================
  
  Filename <- paste( RunYearDir, "/", "Outputs_.RData", sep="" )
  load( Filename )
  Outputs_ <- lapply(Outputs_,function(x) replace(x,is.na(x),0))
  
  #Source in charting functions customized for this outputs file
  #=============================================================
  source( paste( ScriptDir, "/GreenSTEP_Outputs_ChartingTools.r", sep="" ) )
  #prepare some county level summary charts
  chart_loc = paste("chart",yr,'.png',sep = '')
  chart <- makechart("Ig","HhCo2e.CoIgDtDg","number")
  png(chart_loc)
  chart
  dev.off()
  
  #Create and fill a report document for this run year
  #===================================================
  #name and create document
  
  
  outdir = paste("../../views/scenarios/",
                 tail(strsplit(getwd(),"/")[[1]], n = 1),
                 sep="")
  
  
  HTMLStart(outdir=outdir, file=paste("Year", yr, "OutputReport",sep=""),
            extension="html", echo=FALSE, HTMLframe=TRUE)
  
  #add title and some description information about the run
  HTML.title(paste("Year ", yr, " Output Report", sep=""), HR=1)
  HTML.title(paste("Scenario Directory: ", getwd(), "/", RunYearDir, sep=""), HR=3)
  
  #VMT summary
  HTML.title("Daily Vehicle Miles Traveled Summary", HR=2)
  
  #make a table that is a top level summary of dvmt by mode and total
  Mode <- c("Household Vehicles","Bus","Rail","Commercial Service Vehicles","Medium Trucks", "Heavy Trucks")
  DVMT  <- c(sum(Outputs_$Dvmt.CoIgDtDg),
             sum(Outputs_$Dvmt.MaTy[,"Bus"]),
             sum(Outputs_$RailRevMi.Ma * Outputs_$TranAdjFactor / 365),
             Outputs_$CommServ_$CommServDvmt,
             sum(Outputs_$Dvmt.MaTy[,"Truck"]) + Outputs_$NonMpoTruckDvmt) 
  HTML(formattable(c("DVMT"),Mode,list(DVMT),totalrow = TRUE),align="left")
  HTML("DVMT is in units of vehicle miles per day.")
    
  #Fuel and power use summary
  HTML.title("Fuel and Power Use Summary", HR=2)
    
  #make a table that is a top level summary of fuel and power use by mode and total
  fuel <- c(sum(Outputs_$AutoFuel.CoIgDtDg) + sum(Outputs_$LtTruckFuel.CoIgDtDg),
            sum(Outputs_$BusFuel.MaFt),
            0, #No fuel use by trains - all electric
            Outputs_$CommServ_$CommServFuel,
            apply(Outputs_$TruckFuel.MaFt, MARGIN = 3, FUN = sum) + colSums(Outputs_$NonMpoTruckFuel.Ft))
  
  power <- c(sum(Outputs_$AutoPower.CoIgDtDg) + sum(Outputs_$LtTruckPower.CoIgDtDg),
             0, #No power use by buses, no electric buses
             sum(Outputs_$RailPower.Ma),
             Outputs_$CommServ_$CommServKwh,
             0,0) #No power use by trucks, no electric trucks
  
  HTML(formattable(c("Fuel","Power"),Mode,list(fuel,power),totalrow = TRUE),align="left")
  HTML("Fuel is in units of gasoline equivalent gallons per day.")
  HTML("Power is in units of kWh per day.")
    
  #Emissions summary
  HTML.title("Emissions Summary", HR=2)
    
  #make a table that is a top level summary of emissions by mode and total
  emissions <- c(sum(Outputs_$HhCo2e.CoIgDtDg),
                 sum(Outputs_$BusCo2e.Ma),
                 sum(Outputs_$RailCo2e.Ma),
                 Outputs_$CommServ_$CommServCo2e,
                 colSums(Outputs_$TruckCo2e.Ma) + Outputs_$NonMpoTruckCo2e)
  
  HTML(formattable(c("Emissions"),Mode,list(emissions),totalrow = TRUE),align="left")
  HTML("Emissions are in units of tonnes of carbon dioxide equivalents per day.")
  
  #add some county level summary charts
  HTML.title("Charts of Outputs", HR=2)
  
  HTMLInsertGraph(chart_loc)
  
  # HTMLplot(Align="left", Width = 300, GraphRes = 300)
  
  #close the document
  HTMLStop()
  
  #End the loop through years
  #==========================  
#}
rm( Outputs_ )
gc()