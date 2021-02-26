#========================
#GreenSTEP_Outputs_Export.r
#========================

#Copyright 2021, Resource Systems Group, Inc.
#Author: Colin Smith
#Contact: colin.smith@rsginc.com
#Version: 4.0
#Date: 2021-02-26
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


#Description
#===========

#This script exports the contents of Outputs_ (output tabulations) for each RunYear

#Export outputs for the current forecasts year
#=========================

#for( yr in RunYears ) {
  
  print( paste("Exporting ", yr, " outputs to .csv files") )
  RunYearDir <- paste( "outputs/Year", yr, sep="" )
  
  #Load the outputs file
  #=====================
  
  Filename <- paste( RunYearDir, "/", "Outputs_.RData", sep="" )
  load( Filename )

  #Write out each element of the Outputs_ list
  #===========================================
  #note that one of the outputs, CommServ, is a list of different sized elements
  #this needs to be written out seperately by its individual elements
  
  nowrite <- c("CommServ_")
  lapply(names(Outputs_),function(i) if(!i %in% nowrite){write.csv(Outputs_[[i]], file=paste( RunYearDir, "/", i,".csv", sep="" ))})
  lapply(names(Outputs_$CommServ_),function(i) write.csv(Outputs_$CommServ_[[i]], file=paste( RunYearDir, "/", i,".csv", sep="" )))
  
  #End the loop through years
  #==========================  
#}
rm( Outputs_ )
gc()

