#GreenSTEP_Inputs.r
#==================

#Copyright 2010, Oregon Department of Transportation
#Author: Brian Gregor
#Contact: Brian.J.Gregor@odot.state.or.us
#Version: 2.0
#Date: 10/15/10
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


#Description
#===========

#This scripts loads all of the model objects and data needed to run the GreenSTEP model.


#Changes from version 1.1
#========================

# AveRuralDen.Co was moved from being a scenario input to being a model input because it represents base year data not future scenario data. The ave_rural_pop_density.csv file is now housed in the model directory.

# The script now sources in a file that contains global parameter values. These values were previously defined in the GreenSTEP.r script.

# 12-14-09 AB - Edited to handle new format for lttruck_prop which specifies light truck proportions by county as well as year

# Major changes to support data structures required by Version 2 model code. These include new inputs for the travel demand, pricing, and light vehicles models.

#Changes to version 2.0
#----------------------

#1/15/11 BG - The light truck proportions table is by county and year. The object that stored this was improperly called LtTruckProp.Yr. It was changed to LtTruckProp.CoYr.


#Define naming vectors
#=====================

     Abbr_ <- list()
     
	# Year
	Abbr_$Yr <- c( "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025", "2030", "2035", "2040", "2045", "2050" )
  
	# Categories for the age of persons
	Abbr_$Ap <- c( "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus" )

	# Categories for urban types ( metropolitan vs. other urban )
	Abbr_$Ut <- c( "Metropolitan", "Town" )

	# Categories for all development types ( metropolitan, town, rural )
	Abbr_$Dt <- c( "Metropolitan", "Town", "Rural" )

	   # Vehicle type
     Abbr_$Vt <- c( "Auto", "LtTruck", "HvyTruck", "Bus" )
     
     # Income group
     Abbr_$Ig <- c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" )

	# Fuel Type
 	Abbr_$Ft <- c( "ULSD", "Biodiesel", "Gasoline", "Ethanol", "CNG" )
 	
 	# Congestion levels
 	Abbr_$Cl <- c( "None", "Mod", "Hvy", "Sev", "Ext" )

	# Types of vehicles
	Abbr_$Ty <- c("LtVeh","Truck","Bus")
	
	# Functional class of roadways
	Abbr_$Fc <- c("Fwy","Art","Other")

	# Powertrain types
    Abbr_$Pt <- c( "LdIceEco", "LdIceNonEco", "LdHev", "LdFcv", "LdEv", "TruckIce", "TruckEv", "BusIce", "BusEv" )

     
#Read in global values
#=====================

#The global_values.txt file contains various parameters used by the model.

	source( paste( ModelDir,"/global_values.txt", sep="" ) )
    
    
    Census_rNortheast = 0
    Census_rWest = 0
    Census_rSouth = 0
    Census_rMidwest = 0
    
    if (Census_r == "Northeast"){Census_rNortheast = 1}
    if (Census_r == "West"){Census_rWest = 1}
    if (Census_r == "South"){Census_rSouth = 1}
    if (Census_r == "Midwest"){Census_rMidwest = 1}
    
    if (sum(Census_rNortheast,Census_rWest,Census_rSouth,Census_rMidwest) != 1){
        stop("Census_r in global_values.txt must be must in 'Northeast', 'South', 'Midwest' or 'West'")}
	



#Load model starting inventory data
#==================================

#The starting inventories for the model are also contained in the model directory. Once finalized, this data should not change because it reflects existing conditions.

	setwd(ModelDir)
    
    
    GreenSTEP_$HtProb.HtAp <- read.csv("HtProb.HtAp.csv", as.is = TRUE, row.names = 1)
    
    GreenSTEP_$TruckBusAgeDist.AgTy <- read.csv("TruckBusAgeDist.AgTy.csv", as.is = TRUE)
    
    GreenSTEP_$VehProp_ <- list()
    
    
    temp <- read.csv("VehProp_-AgCumProp.AgTy.csv", as.is = TRUE, header = TRUE)
    temp$X <- NULL
    GreenSTEP_$VehProp_$AgCumProp.AgTy <- as.matrix(temp)
    
    GreenSTEP_$VehProp_$AgIgProp.AgIgTy  <- abind(Auto=as.matrix(read.csv("VehProp_-AgIgProp.AgIgTy-Auto.csv", 
                                                              row.name = 1, as.is = TRUE, 
                                                              check.names = FALSE)),
                                      LtTruck=as.matrix(read.csv("VehProp_-AgIgProp.AgIgTy-LtTruck.csv", 
                                                                 row.name = 1, as.is = TRUE, 
                                                                 check.names = FALSE)), 
                                      along=3 )
    rm(temp)
    
    
    GreenSTEP_$VehOwnAdjustments = as.list(as.data.frame(t(read.csv('adj_veh_own.csv', header = FALSE, row.names = 1))))
    
    
    
    
    
    
    
    
    
    attach( GreenSTEP_ )
    
     Model_ <- list()
     
     # Crosswalk between counties and metropolitan areas
     Filename <- "county_groups.csv"
     CountyGroups.. <- read.csv( Filename, as.is=TRUE, row.names=1 )
     Model_$CountyGroups.. <- CountyGroups..
     rm( Filename, CountyGroups.. )

    #Form Abbr_$Co (County) and Abbr_$Ma (Metropolitan areas) from CountyGroups..
    Abbr_$Co <- row.names(Model_$CountyGroups..)
    Abbr_$Ma <- unique(Model_$CountyGroups..$Msa[!is.na(Model_$CountyGroups..$Msa)])

    attach( Abbr_ )


	# Base year metropolitan and non-metropolitan UGB areas by county
	Filename <- "ugb_areas.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Model_$BaseUgbAreas.CoUt <- as.matrix( TempInput.. )[Co,Ut]
	rm( Filename, TempInput.. )

     # Base year metropolitan, town and rural population splits by county
     Filename <- "urban_rural_pop_splits.csv"
     TempInput.. <- read.csv( Filename, row.names=1 )
     Model_$UrbRurPopProp.CoDt <- as.matrix( TempInput.. )[ Co, Dt ]
	rm( Filename, TempInput.. )

     # Base year metropolitan freeway lane miles by metropolitan area
     Filename <- "freeway_lane_miles.csv"
     TempInput.. <- read.csv( Filename )
     Model_$BaseFwyLnMi.Ma <- TempInput..[,2]
     names( Model_$BaseFwyLnMi.Ma ) <- TempInput..[,1]
     rm( Filename, TempInput.. )

     # Base year metropolitan arterial lane miles by metropolitan area
     Filename <- "arterial_lane_miles.csv"
     TempInput.. <- read.csv( Filename )
     Model_$BaseArtLnMi.Ma <- TempInput..[,2]
     names( Model_$BaseArtLnMi.Ma ) <- TempInput..[,1]
     rm( Filename, TempInput.. )

     # Base year metropolitan transit revenue miles by type and metropolitan area
     Filename <- "transit_revenue_miles.csv"
     TempInput.. <- read.csv( Filename, row.names=1 )
     Model_$BaseTranRevMi.. <- TempInput..
     rm( Filename, TempInput.. )
     
	# Base year parameters for metropolitan light vehicle DVMT, truck DVMT proportions,
	# and proportions of DVMT on freeways and arterials
	Filename <- "mpo_base_dvmt_param.csv"
	MpoBaseDvmtParm..Ma <- read.csv( Filename, row.names=1 )
	Model_$MpoBaseDvmtParm..Ma <- MpoBaseDvmtParm..Ma[ Ma, ]
	rm( Filename, MpoBaseDvmtParm..Ma )

	# Proportions of truck and bus DVMT by functional class
	Filename <- "truck_bus_fc_dvmt_split.csv"
	TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
	TruckBusFcDvmtSplit_Va.. <- split( TempInput..[,-1], Va )
     TruckBusFcDvmtSplit_Va.. <- lapply( TruckBusFcDvmtSplit_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Model_$TruckBusFcDvmtSplit_Va.. <- TruckBusFcDvmtSplit_Va..
     rm( Filename, TempInput.., Va, TruckBusFcDvmtSplit_Va.. )

	# Average rural density by county
	Filename <- "ave_rural_pop_density.csv"
	TempInput.. <- read.csv( Filename )
	Model_$AveRuralDen.Co <- TempInput..$Density
	names( Model_$AveRuralDen.Co ) <- TempInput..$County
	rm( Filename, TempInput.. )

     # Factor to convert metropolitan household DVMT to metropolitan light vehicle road DVMT
     if( file.exists( "LtVehDvmtFactor.Ma.RData" ) ) {
          Model_$LtVehDvmtFactor.Ma <- assignLoad( "LtVehDvmtFactor.Ma.RData" )
     } else {
	    Filename <- "hh_dvmt_to_road_dvmt.csv"
	    TempInput.. <- read.csv( Filename )
	    LtVehDvmtFactor.Ma <- TempInput..[,2]
	    names( LtVehDvmtFactor.Ma ) <- TempInput..[,1]
	    Model_$LtVehDvmtFactor.Ma <- LtVehDvmtFactor.Ma[ Ma ]
	    rm( Filename, TempInput.., LtVehDvmtFactor.Ma )
     }
     
	# Set the working directory back to the run directory
	setwd( RunDir )

     attach( Model_ )

#Load the policy inputs
#======================

     setwd( InputDir )

     Inputs_ <- list()
     
	# Statewide real per capita income
	Filename <- "per_cap_inc.csv"
	TempInput.. <- read.csv( Filename )
	Inputs_$PerCapInc.Yr <- TempInput..$Income
	names( Inputs_$PerCapInc.Yr ) <- TempInput..$Year 
	rm( Filename, TempInput.. )

	# Regional proportion of statewide real per capita income
	Filename <- "regional_inc_prop.csv"
	TempInput.. <- read.csv( Filename )
	Inputs_$IncomeProp.Rg <- TempInput..$Proportion
	names( Inputs_$IncomeProp.Rg ) <- TempInput..$Region
	rm( Filename, TempInput.. )
		
     # Metropolitan, town and rural area growth splits by county
     Filename <- "urban_rural_growth_splits.csv"
     TempInput.. <- read.csv( Filename, row.names=1 )
     Inputs_$UrbRurGrowthSplit.CoDt <- as.matrix( TempInput.. )[ Co, Dt ]
	rm( Filename, TempInput.. )
	
	# Growth rates of UGB areas by county and type relative to population growth
	# e.g. a value of 1 means a 10% growth of area for a 10% growth in population
	Filename <- "ugb_area_growth_rates.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Inputs_$UgbAreaGrowthRates.CoUt <- as.matrix( TempInput..)[ Co, Ut ]
	rm( Filename, TempInput.. )

	# Proportions of metropolitan areas living in urban mixed use neighborhoods
	Filename <- "metropolitan_urban_type_proportions.csv"
	Inputs_$UrbanTypeProp.YrMa <- as.matrix( read.csv( Filename, row.names=1 ) )
	rm( Filename )

     # Freeway growth rates by metropolitan area relative to population growth
     Filename <- "fwy_art_growth.csv"
     TempInput.. <- read.csv( Filename )
     Inputs_$FreewayGrowth.Ma <- TempInput..[,2]
     names( Inputs_$FreewayGrowth.Ma ) <- TempInput..[,1]
     Inputs_$ArterialGrowth.Ma <- TempInput..[,3]
     names( Inputs_$ArterialGrowth.Ma ) <- TempInput..[,1]
     rm( Filename, TempInput.. )

     # Per capita transit growth and proportion that is electrified
     Filename <- "transit_growth.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     TransitParm_Va.. <- split( TempInput..[,-1], Va )
     TransitParm_Va.. <- lapply( TransitParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$TransitParm_Va.. <- TransitParm_Va..
     rm( Filename, TempInput.., Va, TransitParm_Va.. )

     # Carshare input parameters
     Filename <- "carshare.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     CarshareParm_Va.. <- split( TempInput..[,-1], Va )
     CarshareParm_Va.. <- lapply( CarshareParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$CarshareParm_Va.. <- CarshareParm_Va..
     rm( Filename, TempInput.., Va, CarshareParm_Va.. )

     # Congestion pricing input parameters
     Filename <- "congestion_charges.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     CongPriceParm_Va.. <- split( TempInput..[,-1], Va )
     CongPriceParm_Va.. <- lapply( CongPriceParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$CongPriceParm_Va.. <- CongPriceParm_Va..
     rm( Filename, TempInput.., Va, CongPriceParm_Va.. )

     # TDM (ECO & IMP) input parameters
     Filename <- "tdm.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     TdmParm_Va.. <- split( TempInput..[,-1], Va )
     TdmParm_Va.. <- lapply( TdmParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$TdmParm_Va.. <- TdmParm_Va..
     rm( Filename, TempInput.., Va, TdmParm_Va.. )

     # Operations program deployment inputs
     Filename <- "ops_deployment.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     OpsDeployParm_Va.. <- split( TempInput..[,-1], Va )
     OpsDeployParm_Va.MaYr <- lapply( OpsDeployParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$OpsDeployParm_Va.MaYr <- OpsDeployParm_Va.MaYr
     rm( Filename, TempInput.., Va, OpsDeployParm_Va.., OpsDeployParm_Va.MaYr )

     # Speed smoothing and ecodriving inputs
     Filename <- "speed_smooth_ecodrive.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     SmoothEcoDriveParm_Va.. <- split( TempInput..[,-1], Va )
     SmoothEcoDriveParm_Va.. <- lapply( SmoothEcoDriveParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$SmoothEcoDriveParm_Va.. <- SmoothEcoDriveParm_Va..
     rm( Filename, TempInput.., Va, SmoothEcoDriveParm_Va.. )

     # Other operations deployment inputs
     Filename <- "other_ops.csv"
     TempInput_ <- as.list( read.csv( Filename, as.is=TRUE ) )
     Ty <- TempInput_[[1]][ c(1,5,9,13) ]
     Lv <- TempInput_[[2]][ 1:4 ]
     OtherOps_Yr.LvTy <- lapply( TempInput_[ 3:length( TempInput_ ) ], function(x) {
          array( x, dim=c( length(Lv), length(Ty) ), dimnames=list( Lv, Ty ) ) } )
     names( OtherOps_Yr.LvTy ) <- gsub( "X", "", names( OtherOps_Yr.LvTy ) )
     Inputs_$OtherOps_Yr.LvTy <- OtherOps_Yr.LvTy
     rm( Filename, TempInput_, OtherOps_Yr.LvTy )                   

     # Eco-driving and low rolling-resistance tire inputs
     Filename <- "eco_tire.csv"
     TempInput.. <- read.csv( Filename, row.names=1 )
     Inputs_$EcoTire..Yr <- TempInput..
     rm( Filename, TempInput.. )

	# Proportions of vehicles that are light trucks
	Filename <- "lttruck_prop.csv"
	TempInput.. <- read.csv( Filename )
	Inputs_$LtTruckProp.CoYr <- TempInput..[,2:ncol(TempInput..)]
	colnames( Inputs_$LtTruckProp.CoYr ) <- gsub("X", "", colnames( Inputs_$LtTruckProp.CoYr ))
	rownames( Inputs_$LtTruckProp.CoYr ) <- TempInput..[,1]
	rm( Filename, TempInput.. )

     # Light weight vehicles input parameters
     Filename <- "light_vehicles.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     LtVehParm_Va.. <- split( TempInput..[,-1], Va )
     LtVehParm_Va.. <- lapply( LtVehParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$LtVehParm_Va.. <- LtVehParm_Va..
     rm( Filename, TempInput.., Va, LtVehParm_Va.. )

	# Private vehicle MPG & electric power consumption (MPKwh) by vehicle year and type
     Filename <- "auto_lighttruck_mpg.csv"
     Inputs_$AutoLtTrkMpg..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
	
	# Proportions of households who optimize
	Filename <- "optimize.csv"
	TempInput.. <- read.csv( Filename )
	Inputs_$OptimProp.Yr <- TempInput..$OptimProp
	names( Inputs_$OptimProp.Yr ) <- TempInput..$Year
	rm( Filename, TempInput.. )

	# Load the plug in hybrid vehicle data
	Filename <- "phev_characteristics.csv"
     Inputs_$PhevRangeProp..Yr <- read.csv( Filename, row.names=1 )
     rm( Filename )

	# Fuel types and carbon emissions by vehicle type
	Filename <- "fuel_co2.csv"
	Inputs_$FuelCo2..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )

	# Auto and light-truck fuel type mix
	Filename <- "auto_lighttruck_fuel.csv"
	Inputs_$AutoLtTrkFuels..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )

	# Carbon emissions per KWH
     Filename <- "power_co2.csv"
     PowerCo2.CoYr <- as.matrix( read.csv( Filename, row.names=1 ) )
     colnames( PowerCo2.CoYr  ) <- gsub( "X", "", colnames( PowerCo2.CoYr ) )
     Inputs_$PowerCo2.CoYr <- PowerCo2.CoYr
     rm( Filename, PowerCo2.CoYr )

     # Costs for fuel, electricity and optional VMT-based charges
     Filename <- "costs.csv"
     Costs..Yr <- read.csv( Filename, row.names=1 )
     Costs.YrCs <- as.matrix( Costs..Yr )
     Inputs_$Costs.YrCs <- Costs.YrCs
	rm( Filename, Costs.YrCs )

     # PAYD input parameters
     Filename <- "payd.csv"
     TempInput.. <- read.csv( Filename, row.names=1 )
     Inputs_$Payd..Yr <- TempInput..
     rm( Filename, TempInput.. )

     # Parking pricing
     Filename <- "parking.csv"
     TempInput.. <- read.csv( Filename )
     Va <- TempInput..$Value
     PkgParm_Va.. <- split( TempInput..[,-1], Va )
     PkgParm_Va.. <- lapply( PkgParm_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm..
     } )
     Inputs_$PkgParm_Va.. <- PkgParm_Va..
     rm( Filename, TempInput.., Va, PkgParm_Va.. )
	
	# Electric vehicle range and proportion of VMT in range traveled by electricity
	Filename <- "ev_characteristics.csv"
     Inputs_$EvRangeProp..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )

	# Bus and train MPG & power consumption by vehicle year and type:
	Filename <- "hvy_veh_mpg_mpk.csv"
	Inputs_$HvyVehMpgMpk..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
	
	# Factors for adjusting 95 percentile age of vehicles
	Filename <- "age_adj.csv"
	Inputs_$AgeAdj.YrTy <- as.matrix( read.csv( Filename, row.names=1 ) )
	rm( Filename )

	# Congestion efficiency of vehicles by year (Yr) and powertrain (Pt)
	Filename <- "cong_efficiency.csv"
	Inputs_$CongEfficiency.YrPt <- as.matrix( read.csv( Filename, row.names=1 ) )
	rm( Filename )

	# Hybrid electric vehicle characteristics
	Filename <- "hev_characteristics.csv"
	Inputs_$HevMpgProp..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
	
	# Truck hybrid electric vehicle characteristics
	Filename <- "hev_truck_characteristics.csv"
	Inputs_$TrkHevMpgProp..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
	
	# Truck AV/CV/platooning and eco-driving
	Filename <- "eco_avcv_truck.csv"
	Inputs_$TrkEcoAVCV..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
	
	# Medium truck fuel type mix
	Filename <- "medium_truck_fuel.csv"
	Inputs_$MedTruckFuels..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
			
	# Heavy truck fuel type mix
	Filename <- "heavy_truck_fuel.csv"
	Inputs_$HvyTruckFuels..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )
	
	# Bus fuel type mix
	Filename <- "bus_fuels.csv"
	TempInput.. <- read.csv( Filename, as.is=TRUE )
	TempInput_ <- split( TempInput.., TempInput..[,1] )
	Fp <- c( "PropGas", "PropCng", "DieselPropBio", "GasPropEth" )
	BusFuels.FpYrMa <- array( 0, dim=c( length( Fp ), length( Yr ), length( Ma ) ),
		dimnames=list( Fp, Yr, Ma ) )
	for( ma in Ma ) {
		DataIn.. <- TempInput_[[ma]]
		DataOut.. <- DataIn..[ , 3:ncol( DataIn.. ) ]
		colnames( DataOut.. ) <- gsub( "X", "", colnames( DataOut.. ) )
		rownames( DataOut.. ) <- DataIn..[ , 2 ]
		BusFuels.FpYrMa[ , , ma ] <- as.matrix( DataOut.. )[ Fp, Yr ]
		rm( DataIn.., DataOut.. )
		}
	Inputs_$BusFuels.FpYrMa <- BusFuels.FpYrMa
	rm( Filename, TempInput.., TempInput_, BusFuels.FpYrMa, ma, Fp )

	# Commercial service vehicle light truck proportions
	Filename <- "comm_service_lttruck_prop.csv"
	TempInput.. <- read.csv( Filename, as.is=TRUE )
	CommServiceLtTruckProp.Yr <- TempInput..$LtTruckProp
	names( CommServiceLtTruckProp.Yr ) <- TempInput..$Year
	Inputs_$CommServiceLtTruckProp.Yr <- CommServiceLtTruckProp.Yr
	rm( Filename, TempInput.., CommServiceLtTruckProp.Yr )

	# Commercial service vehicle fuels
	Filename <- "comm_service_fuel.csv"
	Inputs_$CommServiceFuels..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )

	# Commercial service vehicle EV VMT proportions
	Filename <- "comm_service_pt_prop.csv"
	Inputs_$CommServicePtProp..Yr <- read.csv( Filename, row.names=1 )
	rm( Filename )

	setwd( RunDir )
	
	attach( Inputs_ )

