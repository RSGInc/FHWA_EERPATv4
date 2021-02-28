#===============
# Run_EERPATv4.R
#===============

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

# This script runs the FHWA's Energy and Emissions Reduction Policy Analysis Tool (EERPAT).
# EERPAT is based on Oregon DOT's GreenSTEP model and has been augmented with a freight model, 
# state specific template files, generalizations of some model components that were Oregeon specific,
# and the addition of scripts to further process and summarize outputs.

# This release of EERPAT is designed to be run from the command line in R (or using an IDE such as RStudio)

#Select and Run a Scenario
#=========================
#1. What scenario do you want to run?
#   Update the line below with the name of the scenario folder (case sensitive)
scenario_name <- "Base2010"

#2. change the working directory to the <scenario name> folder
#   you may need to replace "scenarios" in the line of code below with the full path to the scenarios directory
#   if the working directory is something other than the root of FHWA_EERPATv4
#   check with getwd() and update as needed
setwd(file.path("scenarios", scenario_name))

#2. Run the model by sourcing scripts/GreenSTEP.R
source("../../scripts/GreenSTEP.r")
setwd("../../")
