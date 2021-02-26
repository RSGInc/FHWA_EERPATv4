#===========
#GreenSTEP.r
#===========

#Copyright 2009, Oregon Department of Transportation
#Author: Brian Gregor
#Contact: Brian.J.Gregor@odot.state.or.us
#Version: 2.0
#Date: 11/19/10
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


#Description ---------------------------------------------------------------------
#=================================================================================

#This is the main script for running the GreenSTEP Model. (GreenSTEP stands for GREENhouse gas Statewide Transportation Emissions Planning.) The purpose of the GreenSTEP model is to forecast statewide greenhouse gas emissions from the transportation sector in response to policy assumptions about urban growth patterns, public transit and freeway supply, demand management, vehicle replacement, fuel costs and the carbon content of fuels. To date, the model only addresses emissions from surface passenger transportation and trucks. It does not address rail freight transportation, ship or barge transportation, or air transportation. This script sets up run parameters and calls the five modules which implement various portions of the GreenSTEP model. These modules include:

#1) The GreenSTEP_Hh_Synthesis.r module generates synthetic households for each county and year from population forecasts of persons by age group. The synthetic households have characteristics of numbers of persons by each of six age groups and household income. These are saved as RData files to the model directory. This module is run only if it has not been run before because the same populations should be used for all scenarios to reduce stochastic effects on the results.

#2) The GreenSTEP_Firm_Synthesis.r model generates synthetic firms for each county and year from employment forecasts by industry

#3) The GreenSTEP_Inputs.r module loads all of the model objects and data needed to run the GreenSTEP model.

#4) The GreenSTEP_Pop_Road.r module produces POPULATION AND LANE-MILE TABULATIONS FOR ALL YEARS

#5) The GreenStep_Freight.r module performs all calculations for matching buyer-seller firms, allocating freight flows, and simulating mode and vehicle choices for generating medium and heavy truck movements

#6) The GreenSTEP_Sim.r module performs all of the household microsimulation calculations for determining household income, vehicle ownership, household travel and vehicle characteristics and use. The results are aggregated to arrays by county, income and development type and saved to disk.

#7) The GreenSTEP_Sim_Outputs.r module computes summary output tables from the household simulation results. These tables are used by the GreenSTEP_Emissions.r module.

#8) GreenSTEP_Outputs_Export.r exports all of the outputs produced by GreenSTEP_Sim_Outputs.r to .csv files

#9) GreenSTEP_Validation.r produces a county level summary of VMT and vehicle fleet and compares it with observed data

#10) GreenSTEP_Time_Series_Outputs.r produces a summary of model results across all model years executed for the scenario

#Revisions since version 1.1 -----------------------------------------------------
#=================================================================================

#The FuelCostMultiplier was renamed as CostMultiplier. It was converted from a scalar value affecting all households equally into a vector of values by household income group. This was done to reflect differences in the percentage of income paid in taxes and other involuntary income reductions.

#The script eliminates a number of manual steps previously needed to set up a model run.
#- Checks whether all the synthetic population files exist and creates the files if they are not all present
#- Makes clearer distinction between base scenario and policy scenarios. The base scenario just includes the past years (1990-2005). If the user identifies the run as a base scenario, then the run years are automatically defined as 1990 - 2005. If the run is not identified as a base scenario, then the run years are automatically identified as 2010 - 2040 in five year increments. (Note: the user can modify the run years, to include any portion of the years for which there are population forecasts.
#- Automatically copies over the year 2000 values for population by county and development type from the base scenario if it exists. It also copies the values of population by county and year if they exist.

#All parameters were removed from this file and put in data files that are "sourced in". These include run parameters that are contained in the "run_parameters.txt" file in the inputs directory and global parameters that are contained in the "global_values.txt" file in the model directory.

#options(error = recover)

sink(file="session.txt")
sessionInfo()
sink()

#Define function to load packages and download if not installed ------------------
#=================================================================================
#function to test whether a package is available and install without popping up the Cran mirror list  
loadPackage <- function (package) {
  if(!package %in% .packages(all = TRUE)) {
    install.packages(package, repos = "http://cran.r-project.org")
  }
  eval(parse(text=paste("library(", package, ")", sep="")))
}

#Load packages -------------------------------------------------------------------
#=================================================================================
#abind is used in _Inputs
loadPackage("abind")
#packages used in Outputs_ChartingTools
loadPackage("ggplot2")
loadPackage("reshape")
loadPackage("scales")
loadPackage("gsubfn")
#packages used by the freight module
loadPackage("data.table")
loadPackage("rgdal")
loadPackage("rgeos")

#Read in parameters that are unique to the model run -----------------------------
#=================================================================================

#These define whether the scenario is a base scenario (addressing past conditions, e.g. 1990, 1995, 2000 and 2005) or a future scenario, the name of the base scenario, and the years to model. These parameters are stored in a text file in the inputs directory.

	source( "inputs/run_parameters.txt" )

#Identify directory locations for model, inputs, etc. ----------------------------
#=================================================================================

  stateFile <- file("state.txt", "r")
  modeldir <- readLines(stateFile, -1, warn = FALSE)
  state <- substr(modeldir,1,2)
  close(stateFile)
  # Make a list to store the directory references
	Dir_ <- list()

	# Directory references are made with respect to the run directory
	Dir_$RunDir <- getwd()

	# The inputs directory is in the directory where the scenario is run
	Dir_$InputDir <- "inputs"

	# Make directories for the outputs
  if( !file.exists("outputs") ) dir.create( "outputs" )
	for( yr in RunYears ) {
		DirName <- paste( "outputs/Year", yr, sep="" )
		if( !file.exists( DirName ) ) dir.create( DirName )
		rm( yr, DirName )
	}
	Dir_$OutputDir <- "outputs"

	# If it is not the base scenario then copy base year pop and inc tabulations
	if( !IsBaseScenario ) {
		# Define paths to base year pop and inc tabulations
		BaseScenOutputsDir <- paste( "../", BaseScenName, "/outputs/", sep="" )
		BaseYearOutputsDir <- paste( BaseScenOutputsDir, "Year", BaseYear, "/", sep="" )
		PopBaseFileToCopy <- paste( BaseYearOutputsDir, "Pop.CoDt.RData", sep="" )
		IncBaseFileToCopy <- paste( BaseYearOutputsDir, "Inc.CoDt.RData", sep="" )
		# Make a directory to copy information into
		BaseYearCopyDir <- paste( "outputs/Year", BaseYear, sep="" )
		if( !file.exists( BaseYearCopyDir ) ) {
	     	dir.create( BaseYearCopyDir )
	     }
		# Copy base year population and income tabulations
		file.copy( PopBaseFileToCopy, BaseYearCopyDir, overwrite=TRUE )
		file.copy( IncBaseFileToCopy, BaseYearCopyDir, overwrite=TRUE )
    		# Copy population summary by county for all years
		PopYearFileToCopy <- paste( BaseScenOutputsDir, "Pop.CoYr.RData", sep="" )
		file.copy( PopYearFileToCopy, "outputs", overwrite = TRUE )
		rm( BaseScenOutputsDir, BaseYearOutputsDir, PopBaseFileToCopy, IncBaseFileToCopy, 
			PopYearFileToCopy )
	}

	# Directories containing model objects and run scripts are common for all scenarios
	Dir_$ModelDir <- paste("../../model/states/",modeldir,sep = "")
	Dir_$ScriptDir <- "../../scripts"
	attach( Dir_ )

#Define function to load an RData object to an object name -----------------------
#=================================================================================

    assignLoad <- function(filename){
          load(filename)
          get(ls()[ls() != "filename"])
     }

#Load the models, functions, and data that are constant across states ------------
#=================================================================================
     
GreenSTEP_ <- list()
source( paste( ScriptDir, "/GreenSTEP_Models.r", sep="" ) )

#Run the GreenSTEP_Inputs.r script -----------------------------------------------
#=================================================================================

#The GreenSTEP_Inputs.r script loads all of the data objects needed to run the model.

	source( paste( ScriptDir, "/GreenSTEP_Inputs.r", sep="" ) )

#Define bucketRound function -----------------------------------------------------
#=================================================================================

# (method for rounding is constant across states)
bucketRound <- function (x, threshold = 0.5) 
{
  vecf <- floor(x)
  vecd <- x - vecf
  veca <- rep(0, length(x))
  adj <- 0
  for (i in 1:length(x)) {
    adj <- adj + vecd[i]
    if (adj >= threshold) {
      veca[i] <- 1
      adj <- adj - 1
    }
  }
  vecr <- vecf + veca
  return(as.integer(vecr))
} 
	

#Run the GreenSTEP_Hh_Synthesis.r script if necessary ----------------------------
#=================================================================================

#The GreenSTEP_Hh_Synthesis.r script generates "synthetic" households for each county and each year from the county level projections of population by age cohort. It is unnecessary to run this script for every scenario. Once the synthetic households have been generated once, they should be reused for all scenarios to reduce the effects of stochastic variation on the results. Therefore the presence of household input files is checked prior to running this script.

  HsldFiles. <- paste( ModelDir, "/Hsld", Yr, ".RData", sep="" )
	if( !all( file.exists( HsldFiles. ) ) ) {
	     source( paste( ScriptDir, "/GreenSTEP_Hh_Synthesis.r", sep="" ) )
	}
	rm( HsldFiles. )

#Run the GreenSTEP_Firm_Synthesis.r script if necessary --------------------------
#=================================================================================

#The GreenSTEP_Firm_Synthesis.r script generates "synthetic" firms for each county in the simulated state and for each FAF zone outside of the state, for each year. This is done based on county-level firm and employment projections by industry cohort.

	FirmFiles. <- paste( ModelDir, "/Firms", Yr, ".RData", sep="" )
	if( !all( file.exists( FirmFiles. ) ) ) {
	  source( paste( ScriptDir, "/GreenSTEP_Firm_Synthesis.r", sep="" ) )
	}
	rm( FirmFiles. )
  	
#Run the GreenSTEP_Pop_Road.r ----------------------------------------------------
#=================================================================================

#The GreenSTEP_Pop_Road.r module produces POPULATION AND LANE-MILE TABULATIONS FOR ALL YEARS (1990-2050)

  source( paste( ScriptDir, "/GreenSTEP_Pop_Road.r", sep="" ) )
	

#Iterate through the forecast years for the simulation and output production for each year ----
#==============================================================================================
  
for( yr in RunYears ) {

  #=================================
  #RUN THE MODEL FOR A FORECAST YEAR
  #=================================
  #The model is called for the current forecast year.
  
  print( yr )
  OverallStart <- Sys.time()
  
  # Make a directory to save the data for the year in
  RunYearDir <- paste( "outputs/Year", yr, sep="" )
  if( !file.exists( RunYearDir ) ) {
    dir.create( RunYearDir )
  }
  
  #Run the GreenStep_Freight.r script
  #==================================
  
  #The GreenStep_Freight.r module performs all calculations for matching buyer-seller firms, allocating freight flows, and simulating mode and vehicle choices for generating medium and heavy truck movements
  
  source( paste( ScriptDir, "/GreenSTEP_Freight.r", sep="" ) )
  
  #Run the GreenSTEP_Sim.r script
  #==============================

  #The GreenSTEP_Sim.r module performs all of the household microsimulation calculations for determining household income, vehicle ownership, household travel and vehicle characteristics and use. The results are aggregated to arrays by county, income and development type and saved to disk.
      
  source( paste( ScriptDir, "/GreenSTEP_Sim.r", sep="" ) )

  #Run the GreenSTEP_Sim_Outputs.r script
  #======================================

  #The GreenSTEP_Sim_Outputs.r module computes summary output tables from the household simulation results. These tables are used by the GreenSTEP_Emissions.r module and the GreenSTEP_Analysis.r module.

  source( paste( ScriptDir, "/GreenSTEP_Sim_Outputs.r", sep="" ) )

  #Run the GreenSTEP_Outputs_Export.r script
  #=========================================
	
  #GreenSTEP_Outputs_Export.r exports all of the outputs produced by GreenSTEP_Sim_Outputs.r to .csv files
	
  source( paste( ScriptDir, "/GreenSTEP_Outputs_Export.r", sep="" ) )
	

  #run the validation script
  #================================================================================================
  if (IsBaseScenario && (yr == BaseYear)){
    source( paste( ScriptDir, "/GreenSTEP_Validation.r", sep="" ) )  
  }
  
  #END THE LOOP FOR THE YEAR
  #=========================
  
  # Clean up
  gc()
  
}
  	
source( paste( ScriptDir, "/GreenSTEP_Time_Series_Outputs.r", sep="" ) )
