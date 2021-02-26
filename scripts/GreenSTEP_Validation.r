#========================
##GreenSTEP_Validation.r
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

#This script produces a comparison of county level VMT and vehicle fleet outputs with observed data


stateFile <- file("state.txt", "r")
modeldir <- readLines(stateFile, -1, warn = FALSE)
close(stateFile) 

ModelDir <- paste("../../model/states/", modeldir ,sep = "")

if (file.exists(paste(ModelDir, "/validation/counties.csv", sep = ""))) {

    source( "inputs/run_parameters.txt" )
    yr = BaseYear

    RunYearDir <- paste( "outputs/Year", yr, sep="" )

    target_counties <- read.csv(paste(ModelDir, "/validation/counties.csv", sep = ""), row.names = 1)

    load(paste( RunYearDir, "/", "Outputs_.RData", sep="" ))

    #get_estimated_values
    hh_veh_dvmt <- rowSums(Outputs_$Dvmt.CoDt)
    service_veh_dvmt <- rowSums(Outputs_$CommVehDvmt.CoDt)

    validation_output <- data.frame(TargetLightVehicleDvmt = target_counties$TargetLightVehicleDvmt,
                                    EstimatedLightVehicleDvmt = hh_veh_dvmt + service_veh_dvmt,
                                    LightVehicleDvmtError = (hh_veh_dvmt + service_veh_dvmt) / target_counties$TargetLightVehicleDvmt,
                                                    
                                    TargetHhAutoPopulation = target_counties$TargetHhAutoPopulation,
                                    EstimatedHhAutoPopulation = rowSums(Outputs_$NumAuto.CoIgDtDg),
                                    HhAutoPopulationError = rowSums(Outputs_$NumAuto.CoIgDtDg) / target_counties$TargetHhAutoPopulation,
                                    
                                    TargetHhLtTruckPopulation = target_counties$TargetHhLtTruckPopulation,
                                    EstimatedHhLtTruckPopulation = rowSums(Outputs_$NumLtTruck.CoIgDtDg),
                                    HhLtTruckPopulationError = rowSums(Outputs_$NumLtTruck.CoIgDtDg) / target_counties$TargetHhLtTruckPopulation)

    save(validation_output, file = paste( RunYearDir, "/", "ValidationOutput.RData", sep="" ))
    write.csv(validation_output, file = paste( RunYearDir, "/", "ValidationOutput.csv", sep="" ))

} else {

    print ("Skipping validation because data is not available")

}
    
