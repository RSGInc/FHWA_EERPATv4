#========================
#GreenSTEP_Hh_Synthesis.r
#========================

#Copyright 2009, Oregon Department of Transportation
#Author: Brian Gregor
#Contact: Brian.J.Gregor@odot.state.or.us
#Version: 1.4
#Date: 7/15/09
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


#Description
#===========

#This script takes population projections by age cohort, county and year and makes synthetic households that are used in the GreenSTEP model. The attributes of the synthetic households include the number of persons by age category.

#The synthetic households do not need to change for most model runs since their characteristics are not a function of policy variables. Synthetic households only need to be created if there is a different population forecast. The data are saved to disk as RData sets so they can simply be loaded during a GreenSTEP model run in instead of being created from scratch. This can be done in 3 seconds rather than the 3-4 minutes required to create the synthetic households.

#The synthetic household data is saved as a set of .RData files in the "model" directory.


#Create synthetic households for each forecast year and county
#=============================================================

	local( {
	
     	# Iterate by year. A synthetic population file is created for each year.
		for( yr in Yr ) {
		
			# Load the population file for the year
			PopFileName <- paste(ModelDir, "/pop_forecasts/", "pop_by_age_", yr, ".csv", sep="" )
			Pop..CoAp <- read.csv( PopFileName, row.names=1 )
			Co <- rownames( Pop..CoAp )
            
            HhSizeFileName <- paste(ModelDir, "/hhsize_forecasts/", "hh_size_", yr, ".csv", sep="" )
            if (file.exists(HhSizeFileName)){
                    TargetHhSizeDistribution <- read.csv(HhSizeFileName, as.is = TRUE, row.names = 1,
                                                    header = FALSE) 
            } else {
                TargetHhSizeDistribution <- NULL
            }
            
            
            
            
		
          	# Create a list to hold the results where each component of the list
          	# is a matrix of
			Hsld_Co.HhAp <- list()
		
          	# Create the synthetic households for each county
			for( co in Co ) {
		
				# Create households for the county
				Hsld_Co.HhAp[[co]] <- createHhByAge( unlist( Pop..CoAp[co,] ), HtProb.HtAp, TargetHhSizeDistribution[co,]  )[[1]]

			}

          	# Save the results for the year
			SaveFileName <- paste( ModelDir, "/Hsld", yr, ".RData", sep="" )
			save( Hsld_Co.HhAp, file=SaveFileName, compress=TRUE )
			rm( Hsld_Co.HhAp )
		
		}
	
	} )
					 