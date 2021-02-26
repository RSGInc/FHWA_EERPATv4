#GreenSTEP_Sim_Outputs.r
#=======================

#Copyright 2009-2010, Oregon Department of Transportation
#Author: Brian Gregor
#Contact: Brian.J.Gregor@odot.state.or.us
#Version: 2.0
#Date: 12/02/09
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

# Revised on 11/16/09 (AB) to correct for a EV error when LtTrucks existed in the fleet, 
# but no autos for small population counties.
# Also added the 91st output "UrbanHh.CoIgDt"

#Revised on 12/22/10 (JL) to tabulate outputs by County, income group, development type and density group (CoIgDtDg).

#Description
#===========

#This script computes summary output tables from the household simulation results. 


#Produce the outputs for the current forecasts year
#=========================

		print( paste(yr, " outputs running") )
   	print( Sys.time() )
     	
		RunYearDir <- paste( "outputs/Year", yr, sep="" )

        #Make objects to store results
        #=============================

		Outputs_ <- list()
		
		# Define density groupings (other groupings are already defined
		Dg <- c( "0-999", "1000-2499", "2500-4999", "5000-7499", "7500-9999", "10000-14999",
			"15000-19999", "20000-24999", "25000-30000", "30000+" )
		
		# Define urban mixed use designation ( Yes:"1", No: "0" )
		Mx <- c( "0", "1" )
		
		# Define vehicle types
		Ty <- c( "LtVeh", "Truck", "Bus" )
		
		# Define age group
		Ag <- as.factor( as.character(0:32) )
		
		# Define powertrain types (use Pw to avoid conflict with the Pt list)
		Pw <- c( "Ice", "Hev", "Phev", "Ev" )
		
		# Set up dimensions for arrays by county, income group and development type
		OutputDims. <- c( length( Co ), length( Ig ), length( Dt ), length( Dg ) )
		OutputDimnames_ <- list( Co, Ig, Dt, Dg ) 

		# Initialize arrays by county, income group, development type, and density group
		#-------------------------------------------------------------------------
		
		# Arrays that tabulate household characteristics
		Outputs_$Pop.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$Hh.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$UrbanHh.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$DrvAgePop.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$Dvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$LtVehDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$HhTotCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$TotExtCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$VehOwnExp.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$HhParkingCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$HhInc.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$HhCo2e.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
        Outputs_$AveDensity.CoDg <- array( 0, dim=c( length(Co), length(Dg)), dimnames=list(Co,Dg) )
        Outputs_$AveDensity.CoDtDg <- array( 0, dim=c( length(Co), length(Dt), length(Dg) ), dimnames=list(Co,Dt,Dg) )
        Outputs_$MetroAreaDensity.CoDg <- array( 0, dim=c( length(Co), length(Dg)), dimnames=list(Co,Dg) )
        Outputs_$Pop.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)), 
            dimnames=list(Co,Dt,Dg,Mx) )          
        Outputs_$WalkTrips.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)), 
            dimnames=list(Co,Dt,Dg,Mx) )          

		# Arrays that tabulate light vehicle characteristics
		Outputs_$AveAutoVehAge.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$AveLtTruckVehAge.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$NumAuto.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$NumLtTruck.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$AutoEvDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$LtTruckEvDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$AutoHcDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$LtTruckHcDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$AutoFuel.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$LtTruckFuel.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$AutoPower.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$LtTruckPower.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		Outputs_$AutoAge.CoAg <- array( 0, dim=c( length(Co), length(Ag) ), dimnames=list( Co, Ag ) )
		Outputs_$LtTruckAge.CoAg <- array( 0, dim=c( length(Co), length(Ag) ), dimnames=list( Co, Ag ) )
		Outputs_$NumPowertrain.CoDtDgPt <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Pw) ),
			dimnames=list( Co, Dt, Dg, Pw ) ) 
		
		#Arrays for dashboard charts
		
		Dashboard_ <- list()
		
		# Dashboard has columns for (1) all, (2) light, (3) medium + heavy vehicles
		# Rows for: 
		# C02 emissions
		Dashboard_$HhCo2e.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		
		# VMT (all vehicles)
		Dashboard_$Dvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
		
		# VMT (alt fuel vehicles only)
		# Power trains other than ICE
		Dashboard_$DvmtPowertrain.CoDtDgPt <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Pw) ),
		                                          dimnames=list( Co, Dt, Dg, Pw ) )
		
		# CO2eq/mile related to technology/vehicle choice/fuel choice (4th row) (i.e., before cong effects)
		
		# CO2eq/mile related to congestion/operation effects (5th row) (after cong effects - 4th row)
		
		# Energy consumption (to convert everything (liquid fuel consumption and electricity consumption) to the common unit of MegaJoules)
		
		
        #Add data tables that were already summarized at model run time
        #==============================================================
	
        TableNames. <- c( "ArtLnMiCap.Ma", "AveCongTaxPerMi.Ma", "AveSpeed.MaTy", "BusCo2e.Ma",
			"BusFuel.MaFt", "BusRevMi.Ma", "CommServ_", "CommVehDvmt.CoDt", "CommVehDvmt.Ma",
			"CongVmt.ClFcTyMa", "CostSummary.CoVa", "DelayVehHr.MaTy", "Dvmt.CoDt", "Dvmt.MaTy",
			"FfVehHr.MaTy", "FwyLnMiCap.Ma", "Inc.CoDt", "MpgMpkwhAdj.MaPt", "Pop.CoDt",
			"RailCo2e.Ma", "RailPower.Ma", "RailRevMi.Ma", "RoadCostSummary.", "TranRevMiCap.Ma",
			"TruckCo2e.Ma", "TruckFuel.MaFt", "VehHr.MaTy", "NonMpoTruckDvmt", "NonMpoTruckFuel.Ft", 
			"NonMpoTruckCo2e" )
		for( tn in TableNames. ) {
            TableFile <- paste( RunYearDir, "/", tn, ".RData", sep="" )
            Outputs_[[tn]] <- assignLoad( TableFile )
		}
		
		# Add those required by dashboard to Dashboard_
		# C02 emissions
		Dashboard_$CommServ_ <- Outputs_$CommServ_
		Dashboard_$BusCo2e.Ma <- Outputs_$BusCo2e.Ma
		Dashboard_$NonMpoTruckCo2e <- Outputs_$NonMpoTruckCo2e
		Dashboard_$TruckCo2e.Ma <- Outputs_$TruckCo2e.Ma
		
		# VMT (all vehicles)
		Dashboard_$CommVehDvmt.Ma <- Outputs_$CommVehDvmt.CoDt
		Dashboard_$BusRevMi.Ma <- Outputs_$BusRevMi.Ma
		Dashboard_$Dvmt.MaTy <- Outputs_$Dvmt.MaTy
		Dashboard_$NonMpoTruckDvmt <- Outputs_$NonMpoTruckDvmt
		
		# VMT (alt fuel vehicles only)
		# powertrain specific VMT included in Dashboard_$CommServ_ <- Outputs_$CommServ_
		# fuel output for trucks, buses
		
		# CO2eq/mile related to technology/vehicle choice/fuel choice (4th row) (i.e., before cong effects)
		
		# CO2eq/mile related to congestion/operation effects (5th row) (after cong effects - 4th row)
		
		# Energy consumption (to convert everything (liquid fuel consumption and electricity consumption) to the common unit of MegaJoules)
		

	   	#Add input and model parameters that may be used to calculate performance measures
	   	#=================================================================================
	
	   	Outputs_$TranAdjFactor <- TranAdjFactor
	   	Outputs_$BaseTruckVmt <- BaseTruckVmt
	   	Outputs_$BaseYrVmt <- BaseYrVmt
	   	Outputs_$TruckVmtGrowthMultiplier <- TruckVmtGrowthMultiplier
		  
	   	# Add those required by dashboard to Dashboard_
	   	Dashboard_$TranAdjFactor <- TranAdjFactor
	   	  
	   	#Iterate through districts and make summary tables
	   	#=================================================
		for( co in Co ) {       
        #for( co in Co ) { local( {

            # Load county files
            Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
            SynPop.. <- assignLoad( Filename )
            SynPop..$DevType <- factor( SynPop..$DevType, levels=c("Metropolitan", "Town", "Rural") )
            SynPop..$Urban <- factor( as.character( SynPop..$Urban ), levels=c( "0", "1" ) )

            # Calculate average densities
            #============================
            
            # Classify density group of each household
            DgCut <- c( 0, 1000, 2500, 5000, 7500, 10000, 15000, 20000, 25000, 30000, max(SynPop..$Htppopdn) )
            DenGroup. <- cut( SynPop..$Htppopdn, DgCut, right=FALSE, labels=Dg )   				

            # Compute the average density by density-group 
            Density.Dg <- tapply( SynPop..$Htppopdn, DenGroup., mean)[Dg]
            Density.Dg[ is.na( Density.Dg ) ] <- 0
            Outputs_$AveDensity.CoDg[ co, ] <- Density.Dg
            #Outputs_$AveDensity.CoDg[ co, ] <- Density.Dg

            # Compute average density by density-group and development type
            Density.DtDg <- tapply( SynPop..$Htppopdn, list( SynPop..$DevType, DenGroup. ), mean )[Dt,Dg]
            Density.DtDg[ is.na( Density.DtDg ) ] <- 0
            Outputs_$AveDensity.CoDtDg[ co, , ] <- Density.DtDg  
            #Outputs_$AveDensity.CoDtDg[ co, , ] <- Density.DtDg  

            # Compute the metro-area density by density-group
            Outputs_$MetroAreaDensity.CoDg[ co, ] <- Density.DtDg[ "Metropolitan", ]
            #Outputs_$MetroAreaDensity.CoDg[ co, ] <- Density.DtDg[ "Metropolitan", ]

            # Tabulate household characteristics
            #===================================

            # Household population
            Pop.Hh <- SynPop..$Hhsize
            Pop.3d <- tapply( Pop.Hh, list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ),
                function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            Pop.3d[ is.na( Pop.3d ) ] <- 0
  			Outputs_$Pop.CoIgDtDg[ co, , , ] <- Pop.3d
  			#Outputs_$Pop.CoIgDtDg[ co, , , ] <- Pop.3d

            # Household population by density group and urban mixed use designation
            Pop.3d <- tapply( Pop.Hh, list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
            Pop.3d[ is.na( Pop.3d ) ] <- 0
            Outputs_$Pop.CoDtDgMx[ co, , , ] <- Pop.3d
			#Outputs_$Pop.CoDtDgMx[ co, , , ] <- Pop.3d

            # Number of households
            Tab1.3d <- table( SynPop..$IncGrp, SynPop..$DevType, DenGroup. )[Ig,Dt,Dg]
			Outputs_$Hh.CoIgDtDg[ co, , , ] <- Tab1.3d
			#Outputs_$Hh.CoIgDtDg[ co, , , ] <- Tab1.3d
			
            print(co)
            print(sum(Tab1.3d))
            print(sum(Outputs_$Hh.CoIgDtDg))
            
			# Number of Urban households              
            Tab1.3d <- tapply( as.numeric( as.character( SynPop..$Urban ) ), 
                list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), sum )[Ig,Dt,Dg]
            Tab1.3d[ is.na( Tab1.3d ) ] <- 0
            Outputs_$UrbanHh.CoIgDtDg[ co, , , ] <- Tab1.3d					   
            #Outputs_$UrbanHh.CoIgDtDg[ co, , , ] <- Tab1.3d					   
    				
			# Driver age population
            VehHhDrvPop.3d <- tapply( SynPop..$DrvAgePop, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
			VehHhDrvPop.3d[ is.na( VehHhDrvPop.3d ) ] <- 0
			Outputs_$DrvAgePop.CoIgDtDg[ co, , , ] <- VehHhDrvPop.3d
			#Outputs_$DrvAgePop.CoIgDtDg[ co, , , ] <- VehHhDrvPop.3d

            # Dvmt 
            Dvmt.3d <- tapply( SynPop..$Dvmt, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            Dvmt.3d[ is.na( Dvmt.3d ) ] <- 0				
            Outputs_$Dvmt.CoIgDtDg[ co, , , ] <- Dvmt.3d
            #Outputs_$Dvmt.CoIgDtDg[ co, , , ] <- Dvmt.3d
            Dashboard_$Dvmt.CoIgDtDg[ co, , , ] <- Dvmt.3d

            # Light Vehicle Dvmt 
            LtVehDvmt.3d <- tapply( SynPop..$LtVehDvmt, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            LtVehDvmt.3d[ is.na( LtVehDvmt.3d ) ] <- 0				
            Outputs_$LtVehDvmt.CoIgDtDg[ co, , , ] <- LtVehDvmt.3d
            #Outputs_$LtVehDvmt.CoIgDtDg[ co, , , ] <- Dvmt.3d

            # Household total cost                                                                     
            TotCost.Hh <- SynPop..$HhTotCost
            TotCost.3d <- tapply( TotCost.Hh, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            TotCost.3d[ is.na( TotCost.3d ) ] <- 0				
            Outputs_$HhTotCost.CoIgDtDg[ co, , , ] <- TotCost.3d
            #Outputs_$HhTotCost.CoIgDtDg[ co, , , ] <- TotCost.3d

            # Household external cost                                                                     
            TotExtCost.Hh <- SynPop..$TotExtCost
            TotExtCost.3d <- tapply( TotExtCost.Hh, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            TotExtCost.3d[ is.na( TotExtCost.3d ) ] <- 0				
            Outputs_$TotExtCost.CoIgDtDg[ co, , , ] <- TotExtCost.3d
            #Outputs_$TotExtCost.CoIgDtDg[ co, , , ] <- TotExtCost.3d

            # Household vehicle ownership cost                                                                     
            VehOwnExp.Hh <- SynPop..$VehOwnExp
            VehOwnExp.3d <- tapply( VehOwnExp.Hh, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            VehOwnExp.3d[ is.na( VehOwnExp.3d ) ] <- 0				
            Outputs_$VehOwnExp.CoIgDtDg[ co, , , ] <- VehOwnExp.3d
            #Outputs_$VehOwnExp.CoIgDtDg[ co, , , ] <- VehOwnExp.3d

    		# Household parking cost
            ParkingCost.Hh <- SynPop..$DailyPkgCost 
            ParkingCost.3d <- tapply( ParkingCost.Hh, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            ParkingCost.3d[ is.na( ParkingCost.3d ) ] <- 0				                
            Outputs_$HhParkingCost.CoIgDtDg[ co, , , ] <- ParkingCost.3d
            #Outputs_$HhParkingCost.CoIgDtDg[ co, , , ] <- ParkingCost.3d

            # Household income
            HhInc.3d <- tapply( SynPop..$Hhincttl, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            HhInc.3d[ is.na( HhInc.3d ) ] <- 0				
            Outputs_$HhInc.CoIgDtDg[ co, , , ] <- HhInc.3d
            #Outputs_$HhInc.CoIgDtDg[ co, , , ] <- HhInc.3d
                                                                                
            # Household CO2e
            Co2e.Hh <- SynPop..$FuelCo2e + SynPop..$ElecCo2e   
            Co2e.3d <- tapply( Co2e.Hh, list( SynPop..$IncGrp,
                SynPop..$DevType, DenGroup. ), function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
            Co2e.3d[ is.na( Co2e.3d ) ] <- 0				
            Outputs_$HhCo2e.CoIgDtDg[ co, , , ] <- Co2e.3d
            #Outputs_$HhCo2e.CoIgDtDg[ co, , , ] <- Co2e.3d
            Dashboard_$HhCo2e.CoIgDtDg[ co, , , ] <- Co2e.3d

			# Household walk trips
            AveWalkTrips.3d <- tapply( SynPop..$AveWalkTrips, list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
            AveWalkTrips.3d[ is.na( AveWalkTrips.3d ) ] <- 0
            Outputs_$WalkTrips.CoDtDgMx[ co, , , ] <- AveWalkTrips.3d
			#Outputs_$WalkTrips.CoDtDgMx[ co, , , ] <- AveWalkTrips.3d

            # Tabulate vehicle characteristics
            #=================================

            HasVeh.Hh <- SynPop..$Hhvehcnt >= 1   

            # Tabulate average vehicle age
            Tab.4d <- tapply( unlist( SynPop..$VehAge[ HasVeh.Hh ] ),
                list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ), rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                rep( DenGroup., SynPop..$Hhvehcnt ), unlist( SynPop..$VehType[HasVeh.Hh] ) ), mean )[Ig,Dt,Dg,Vt[1:2]]
            Tab.4d[ is.na( Tab.4d ) ] <- 0
            Outputs_$AveAutoVehAge.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            Outputs_$AveLtTruckVehAge.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "LtTruck" ]
            #Outputs_$AveAutoVehAge.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            #Outputs_$AveLtTruckVehAge.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "LtTruck" ]

            # Tabulate number of vehicles
            Tab.4d <- table( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ),
                rep( SynPop..$DevType, SynPop..$Hhvehcnt ), rep( DenGroup., SynPop..$Hhvehcnt ),
                unlist( SynPop..$VehType[ HasVeh.Hh] ) )[Ig,Dt,Dg,Vt[1:2]]
            Tab.4d[ is.na( Tab.4d ) ] <- 0
        	Outputs_$NumAuto.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
        	Outputs_$NumLtTruck.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
        	#Outputs_$NumAuto.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
        	#Outputs_$NumLtTruck.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
 
            # Tabulate vehicle DVMT for autos and light trucks
            # Tabulate EV DVMT by income group, development type, and density group
            Tab.4d <- tapply( unlist( SynPop..$EvVehDvmt[ HasVeh.Hh ] ),
                list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ), rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                rep( DenGroup., SynPop..$Hhvehcnt ), 
                unlist( SynPop..$VehType[ HasVeh.Hh ] ) ), sum )[Ig,Dt,Dg,Vt[1:2]]
            Tab.4d[ is.na( Tab.4d ) ] <- 0
            Outputs_$AutoEvDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            Outputs_$LtTruckEvDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
            #Outputs_$AutoEvDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            #Outputs_$LtTruckEvDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
				
            # Tabulate HC-vehicle DVMT
            Tab.4d <- tapply( unlist( SynPop..$HcVehDvmt[ HasVeh.Hh] ),
                list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ), rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                rep( DenGroup., SynPop..$Hhvehcnt ),				     
                unlist( SynPop..$VehType[ HasVeh.Hh] ) ), sum )[Ig,Dt,Dg,Vt[1:2]]
            Tab.4d[ is.na( Tab.4d ) ] <- 0
            Outputs_$AutoHcDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            Outputs_$LtTruckHcDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
            #Outputs_$AutoHcDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            #Outputs_$LtTruckHcDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]

            # Tabulate vehicle fuel (gas vehicle and PHEV)
            HcVehDvmt. <- unlist( SynPop..$HcVehDvmt[ HasVeh.Hh ] )
            HcVehDvmt.[ is.na( HcVehDvmt. ) ] <- 0
            VehMpg. <- unlist( SynPop..$VehMpg[ HasVeh.Hh ] )
            VehMpg.[ is.na( VehMpg. ) ] <- 0
            VehFuel. <- HcVehDvmt. / VehMpg.
            VehFuel.[ is.nan( VehFuel. ) ] <- 0
            Tab.4d <- tapply( VehFuel., list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ),
                rep( SynPop..$DevType, SynPop..$Hhvehcnt), rep( DenGroup., SynPop..$Hhvehcnt ),				     
                unlist( SynPop..$VehType[ HasVeh.Hh ] ) ), sum )[Ig,Dt,Dg,Vt[1:2]]
            Tab.4d[ is.na( Tab.4d ) ] <- 0
           	Outputs_$AutoFuel.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
           	Outputs_$LtTruckFuel.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
           	#Outputs_$AutoFuel.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
           	#Outputs_$LtTruckFuel.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]

  			# Tabulate vehicle power (PHEV and EV) by income group, development type, and density group
            EvVehDvmt. <- unlist( SynPop..$EvVehDvmt[ HasVeh.Hh ] )
            EvVehDvmt.[ is.na( EvVehDvmt. ) ] <- 0
            VehMpkwh. <- unlist( SynPop..$VehMpkwh[ HasVeh.Hh ] )
            VehPower. <- EvVehDvmt. / VehMpkwh.
            VehPower.[ is.na( VehMpkwh. ) ] <- 0
            Tab.4d <- tapply( VehPower., list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ),
                rep( SynPop..$DevType, SynPop..$Hhvehcnt), rep( DenGroup., SynPop..$Hhvehcnt ),				     
                unlist( SynPop..$VehType[ HasVeh.Hh ] ) ), sum )[Ig,Dt,Dg,Vt[1:2]]
            Tab.4d[ is.na( Tab.4d ) ] <- 0
            Outputs_$AutoPower.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            Outputs_$LtTruckPower.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
            #Outputs_$AutoPower.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
            #Outputs_$LtTruckPower.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]

			# Tabulate automobile and light truck age distributions
			Tab.2d <- table( unlist( SynPop..$VehAge[ HasVeh.Hh ] ), unlist( SynPop..$VehType[ HasVeh.Hh ] ) )
			Outputs_$AutoAge.CoAg[ co, rownames( Tab.2d ) ] <- Tab.2d[ , "Auto" ]          
			Outputs_$LtTruckAge.CoAg[ co, rownames( Tab.2d ) ] <- Tab.2d[ , "LtTruck" ]              
			#Outputs_$AutoAge.CoAg[ co, ] <- Tab.2d[ , "Auto" ]          
			#Outputs_$LtTruckAge.CoAg[ co, ] <- Tab.2d[ , "LtTruck" ]              

			# Tabulate number of vehicles by Powertrain
            Tab.3d <- table(    factor(rep( SynPop..$DevType, SynPop..$Hhvehcnt ),Dt),
                                factor(rep( DenGroup., SynPop..$Hhvehcnt ), Dg),
                                factor(unlist( SynPop..$Powertrain[ HasVeh.Hh]),Pw)
                                )[Dt,Dg,Pw]
            Tab.3d[ is.na( Tab.3d ) ] <- 0
        	Outputs_$NumPowertrain.CoDtDgPt[ co, , , ] <- Tab.3d
        	#Outputs_$NumPowertrain.CoDtDgPt[ co, , , ] <- Tab.3d
      
      # Tabulate dvmt by Powertrain
        	Tab.3d <- tapply( unlist( SynPop..$VehDvmt[ HasVeh.Hh ] ), 
        	                  list( rep( SynPop..$DevType, SynPop..$Hhvehcnt), rep( DenGroup., SynPop..$Hhvehcnt ),				     
        	                        factor(unlist( SynPop..$Powertrain[ HasVeh.Hh]),Pw)), sum )[Dt,Dg,Pw]
        	Tab.3d[ is.na( Tab.3d ) ] <- 0
        
        	Dashboard_$DvmtPowertrain.CoDtDgPt[ co, , , ] <- Tab.3d	
        	  	      
            
            #tabulate
            Outputs_$LtVehDvmt.Co <- rowSums(Outputs_$CommVehDvmt.CoDt + Outputs_$Dvmt.CoDt)
            
            
			
		# Close local function
		#} ) 
     
		gc()

		# Close loop through counties 	
		}

	#Save the results
	#================

		Filename <- paste( RunYearDir, "/", "Outputs_.RData", sep="" )
		save( Outputs_, file=Filename )
		Filename <- paste( RunYearDir, "/", "Inputs_.RData", sep="" )
		save( Inputs_, file=Filename )
		Filename <- paste( RunYearDir, "/", "Model_.RData", sep="" )
		save( Model_, file=Filename )
		Filename <- paste( RunYearDir, "/", "Dashboard_.RData", sep="" )
		save( Dashboard_, file=Filename )

		print( Sys.time() )

	rm( Outputs_ )
	rm( Dashboard_ )
	
  gc()

	