#GreenSTEP_Time_Series_Outputs.r
#===============================

#Copyright 2017, Resource Systems Group, Inc.
#Author: Colin Smith
#Contact: colin.smith@rsginc.om
#Version: 0.2
#Date: 1/24/17
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

#Description
#===========

#This script produces a set of summaries across all run years executed for the current sceario
#Several of the summaries are combined across scenarios and plotted in the dashboard

# Initialize Summaries for export to CSV
#-----------------------------------------

#	Statewide CO2 equivalent, DVMT, and fuel consumption
StateCO2 <- c()
StateDvmt <- c()
Fuel <- c()

#	Couty DVMT for Household vehicles and commercial service vehicles
DVMThhCommVehbyCounty <- c()

#	 for metropolitan areas, towns, and rural areas
DVMTbyDt <- c()

#	DVMT by transportation modes - auto, commercial vehicle, light truck, bus, and med/heavy trucks
mode <- c("Household","CommServ","Bus","Rail","Truck")
DVMTbyMode <- matrix(nrow=sum(!is.na(mode)),ncol=sum(!is.na(RunYears)))
rownames(DVMTbyMode) <- mode

#	CO2 emissions from different transportation modes
CO2byMode <- matrix(nrow=length(mode),ncol=sum(!is.na(RunYears)))
rownames(CO2byMode)<- c("Bus","Commercial Service Vehicle","Household","Truck","Rail")

#	Household CO2 by county and by develpment types
HhCO2byCounty <- c()
HhCO2byDt <- c()

#	Vehicle population of cars, light trucks and power trains for calibration
NumAutoLtTruckPowerTrain <- matrix(nrow=6,ncol=sum(!is.na(RunYears)))
rownames(NumAutoLtTruckPowerTrain) <- c("Auto","LtTruck","ICE","HEV","PHEV","EV")
#	Fuel by mode
FuelMode <- matrix(nrow=5,ncol=sum(!is.na(RunYears)))
rownames(FuelMode) <-c("Auto","Bus","Commserv","Lttruck","Truck")

# Initialize Summaries for Dashboard
#-----------------------------------------

# Columns for (1) all, (2) light, (3) medium + heavy vehicles
# Rows for 

# C02 emissions
LightCO2 <- c()
HeavyCO2 <- c()

# VMT (all vehicles)
LightVMT <- c()
HeavyVMT <- c()

# VMT (alt fuel vehicles only)
LightAltVMT <- c()
HeavyAltVMT <- c()

# CO2eq/mile related to technology/vehicle choice/fuel choice (4th row) (i.e., before cong effects)


# CO2eq/mile related to congestion/operation effects (5th row) (after cong effects - 4th row)


# Energy consumption (to convert everything (liquid fuel consumption and electricity consumption) to the common unit of MegaJoules)



# Loop through years and process Outputs_
#-----------------------------------------

ycount <- 1
for( yr in RunYears ) {

# Looping through all output year folders
    
	RunYearDir <- paste( "outputs/Year", yr, sep="" )
  Filename <- paste( RunYearDir, "/", "Outputs_.RData", sep="" )
	output <- assignLoad( Filename )
	output <- lapply(output,function(x) replace(x,is.na(x),0))
   
	# Summaries for export to CSV
	#-----------------------------------------
	
	StateCO2 <- c(StateCO2, (sum(output$HhCo2e.CoIgDtDg,output$BusCo2e.Ma,output$CommServ_$CommServCo2e,
				output$NonMpoTruckCo2e,output$RailCo2e.Ma,output$TruckCo2e.Ma)))
				
	StateDvmt <- c(StateDvmt, sum(sum(output$Dvmt.CoIgDtDg),
				sum(output$CommVehDvmt.CoDt),
				sum(output$BusRevMi.Ma * output$TranAdjFactor / 365),
				sum(output$RailRevMi.Ma * output$TranAdjFactor / 365),
				sum(output$Dvmt.MaTy[,"Truck"])+ sum(output$NonMpoTruckDvmt)))
	
	Fuel <- c(Fuel, sum(output$AutoFuel.CoIgDtDg, output$BusFuel.MaFt, output$CommServ_$CommServFuel, output$LtTruckFuel.CoIgDtDg, sum(output$TruckFuel.MaFt)+ sum(output$NonMpoTruckFuel.Ft)))
	
	DVMThhCommVehbyCounty <- cbind(DVMThhCommVehbyCounty, (apply(output$Dvmt.CoDt,1,sum)+apply(output$CommVehDvmt.CoDt,1,sum)))
	
	DVMTbyDt <- cbind(DVMTbyDt,
				c(sum(output$Dvmt.MaTy[,"LtVeh"]),sum(output$Dvmt.CoIgDtDg)+sum(output$CommVehDvmt.CoDt)-sum(output$Dvmt.MaTy[,"LtVeh"])) +
				  c(sum(output$Dvmt.MaTy[,"Truck"]),sum(output$NonMpoTruckDvmt)) +
				  c(sum(output$BusRevMi.Ma * output$TranAdjFactor / 365),0))

	DVMTbyMode [,ycount] <- c(sum(output$Dvmt.CoIgDtDg),sum(output$CommVehDvmt.CoDt),
				sum(output$BusRevMi.Ma * output$TranAdjFactor / 365),
				sum(output$RailRevMi.Ma * output$TranAdjFactor / 365),
				sum(output$Dvmt.MaTy[,"Truck"])+ sum(output$NonMpoTruckDvmt))
		
	CO2byMode [,ycount] <- c(sum(output$BusCo2e.Ma),sum(output$CommServ_$CommServCo2e),sum(output$HhCo2e.CoIgDtDg),
		sum(output$NonMpoTruckCo2e)+sum(output$TruckCo2e.Ma),sum(output$RailCo2e.Ma))
	
	HhCO2byCounty <- cbind(HhCO2byCounty,apply(output$HhCo2e.CoIgDtDg,1,sum))
	HhCO2byDt <- cbind(HhCO2byDt,apply(output$HhCo2e.CoIgDtDg,3,sum))
	
	NumAutoLtTruckPowerTrain [,ycount] <- c(sum(output$NumAuto.CoIgDtDg),sum(output$NumLtTruck.CoIgDtDg),sum(output$NumPowertrain.CoDtDgPt[,,,"Ice"]),sum(output$NumPowertrain.CoDtDgPt[,,,"Hev"]),sum(output$NumPowertrain.CoDtDgPt[,,,"Phev"]),sum(output$NumPowertrain.CoDtDgPt[,,,"Ev"]))
	
	FuelMode [,ycount] <- c(sum(output$AutoFuel.CoIgDtDg), sum(output$BusFuel.MaFt), sum(output$CommServ_$CommServFuel), sum(output$LtTruckFuel.CoIgDtDg), sum(output$TruckFuel.MaFt)+ sum(output$NonMpoTruckFuel.Ft))
	
	# Initialize Summaries for Dashboard
	#-----------------------------------------
	
	# C02 emissions
	# Personal light vehicles, commercial service light vehicles
	LightCO2 <- c(LightCO2, sum(output$HhCo2e.CoIgDtDg, output$CommServ_$CommServCo2e))
	# Buses, trucks outside  metro areas and trucks inside metro areas
	HeavyCO2 <- c(HeavyCO2, sum(output$BusCo2e.Ma, output$NonMpoTruckCo2e, output$TruckCo2e.Ma))
  # These are onroad emissions, rail transit related emissions are omitted.
	
	# VMT (all vehicles)
	# Personal light vehicles, commercial service light vehicles
	LightVMT <- c(LightVMT, sum(output$Dvmt.CoIgDtDg, output$CommVehDvmt.CoDt))
	# Buses, trucks outside  metro areas and trucks inside metro areas
	HeavyVMT <- c(HeavyVMT, sum(output$BusRevMi.Ma * output$TranAdjFactor / 365, output$Dvmt.MaTy[,"Truck"], output$NonMpoTruckDvmt))
	# These are onroad emissions, rail transit related emissions are omitted.

	# VMT (alt fuel vehicles only)
	# LightAltVMT <- c(LightAltVMT, )
	# HeavyAltVMT <- c(HeavyAltVMT, )
	
	
	#Increment year counter
	ycount=ycount+1
}

# Format and save outputs
#-----------------------------------------

AllState <- rbind(CO2 = StateCO2, Dvmt = StateDvmt, Fuel)
colnames(AllState) <- RunYears
colnames(DVMThhCommVehbyCounty) <- RunYears
colnames(DVMTbyDt) <- RunYears
rownames(DVMTbyDt) <- c("Metro","NonMetro")
colnames(DVMTbyMode) <- RunYears
colnames(CO2byMode) <- RunYears
colnames(HhCO2byCounty) <- RunYears
colnames(HhCO2byDt) <- RunYears
colnames(NumAutoLtTruckPowerTrain) <- RunYears
colnames(FuelMode) <- RunYears

write.csv(AllState, paste(OutputDir, "AllStateDvmtCO2Fuel.csv", sep = "/"))
write.csv(DVMThhCommVehbyCounty, paste(OutputDir, "CountyHHCommerVehDVMT.csv", sep = "/"))
write.csv(DVMTbyDt, paste(OutputDir, "DevelopmentTypeDVMT.csv", sep = "/"))
write.csv(DVMTbyMode, paste(OutputDir, "ModeDVMT.csv", sep = "/"))
write.csv(CO2byMode, paste(OutputDir, "ModeCO2.csv", sep = "/"))
write.csv(HhCO2byCounty, paste(OutputDir, "HouseholdCO2byCounty.csv", sep = "/"))
write.csv(HhCO2byDt,paste(OutputDir, "HouseholdC02byDevelopmentType.csv", sep = "/"))
write.csv(NumAutoLtTruckPowerTrain, paste(OutputDir, "VehiclePopulation.csv", sep = "/"))
write.csv(FuelMode,paste(OutputDir, "FuelConsumptionByMode.csv", sep = "/"))



