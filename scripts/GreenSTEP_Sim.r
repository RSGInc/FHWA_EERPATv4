#===============
#GreenSTEP_Sim.r
#===============

#Copyright 2009-2010, Oregon Department of Transportation
#Author: Brian Gregor
#Contact: Brian.J.Gregor@odot.state.or.us
#Version: 2.0
#Date: 1/15/11
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


#Description
#===========

#This module performs all of the household microsimulation calculations for determining household income, vehicle ownership, household travel and vehicle characteristics and use. The results are aggregated to arrays by county, income and development type and saved to disk.


#Changes from version 1.1
#========================

# Calculation of average rural density now assumes that new rural development will occur at a population density of 120 persons per square mile. This density corresponds to a minimum lot size of 2 acres. A weighted average is computed from the base year rural density and base year rural population and the 120 person per square mile density and rural population growth.

# Code on applying the cost multiplier is changed to reflect that the multiplier is now a vector with a different value for each income group.

# The calculation of income adjustments was moved out of the function for calculating costs and into this script.

# The function for calculating household DVMT was changed from the previous which predicted one day's VMT to a function to predict an average day's VMT for the household.

# Corrected bug where condition test needed presence of PopGrowth.Dt object before it was defined.

# Corrected calculation of driving age population. The code previously was adding up the entire household population. Now it correctly just adds population 15 and older.

# Corrected urban type calculation to use urban type proportion input rather than default NA

# 11/3/09 AB: Corrected input arguments to calculation of light truck proportions to read the appropriate input data.

# 12/14/09 AB:  Corrected to reference new "county" breakout for lttruck_prop input

# 12/02/10 BG: Major upgrade to implement the version 2.0 of the GreenSTEP model. This version does all of the household computations on an individual household basis. Most notably, greenhouse gas calculations are done for individual households rather than for collections of households as was done previously. This provides more flexibility for reporting outputs. Another important difference is that the model flow has been changed to implement the new household budget-based approach to estimating the effects of costs on vehicle travel. 

#Changes to Version 2.0
#----------------------

# 1/15/11 BG: Changed calls to predictLtTruckOwn and calcVehAges functions. These were called with NA default arguments for the TruckProp and AdjRatio arguments respectively. This resulted in no adjustments to vehicle fleet ages regardless of the inputs. These now use the input values for these arguments. Also changed the name of the object referred to in the name passed to TruckProp from LtTruckProp.Yr to LtTruckProp.CoYr.

# Added a calculation of the annual statewide costs for adding freeway and arterial lane miles in metropolitan areas.

#============================================================================================
#STEP 1: POPULATE THE HOUSEHOLD DATA WITH INCOME, LAND USE AND TRANSPORTATION CHARACTERISTICS
#============================================================================================

  #Step 1a: Load synthetic population and create summaries
  #=======================================================
  
    # Load the synthetic population file for the year
    setwd(ModelDir)
    PopFileName <- paste( "Hsld", yr, ".RData", sep="" )
    SynPop_ <- assignLoad( PopFileName )
    rm( PopFileName )
    setwd( RunDir )
  
  	# Make a matrix to store population sums by county and development type
    Pop.CoDt <- array( 0, dim=c( length( Co ), length( Dt ) ), dimnames=list( Co, Dt ) )
  
    # Calculate the power-transformed per capita income by county       
    PowPerCapInc.Co <- calcCountyPowPerCapInc( PerCapInc.Yr, IncomeProp.Rg,
                                               CountyGroups.., yr )
         
    # Load the base year population if yr is not the base year
    if( yr != BaseYear ) {
    	BasePop.CoDt <- assignLoad( paste("outputs/Year",BaseYear,"/Pop.CoDt.RData", sep="") )
    }
  
    # Initialize a matrix to hold the total household income by county and development type
    Inc.CoDt <- Pop.CoDt * 0	
  
  #Step 1b: Calculate unit emissions for fuel and electricity for year
  #===================================================================
  
  	# Calculate average fuel CO2e per gallon
  	AveFuelCo2e. <- calcAveFuelCo2e( yr, Fuels..Yr=AutoLtTrkFuels..Yr,
  	                                 Co2..Yr=FuelCo2..Yr, MjPerGallon=121, OutputType="MetricTons" )
  
  	# Calculate average electricity CO2e per Kwh
  	AveElectricCo2e.Co <- calcAveElectricCo2e( yr, Co2.CoYr=PowerCo2.CoYr,
  	                                           OutputType="MetricTons" )
  
  #Step 1c: Iterate through each county and add income, land use and driver information
  #====================================================================================
  
    StartTime <- Sys.time()
    print( "Adding income transform, density & vehicle ownership variables" )
  
    for( co in Co ) {
  
    	print( co )
    	
      # Wrap inside a local function to reduce clutter and conflict in global env
      local( {
  
        # Extract the synthetic population for the county
        SynPop.. <- data.frame( SynPop_[[co]] )
  
  			# Give each household a unique id
  			SynPop..$Houseid <- 1:nrow( SynPop.. )
  			
  			# Calculate the household size
  			SynPop..$Hhsize <- rowSums( SynPop..[ , Ap ] )
  			
  			# Add the power transformed per capita income for the county
  			SynPop..$PowPerCapInc <- PowPerCapInc.Co[ co ]
  
  			# Predict household income
  			SynPop..$Hhincttl <- predictIncome( SynPop.., IncomeModel[[state]] )
  			MinInc <- quantile( SynPop..$Hhincttl, prob=0.01 )
  			SynPop..$Hhincttl[ SynPop..$Hhincttl < MinInc ] <- MinInc
     
        # Classify households according to income group
        MaxInc <- max( SynPop..$Hhincttl )
        IncBreaks. <- c( 0, 20000, 40000, 60000, 80000, 100000, MaxInc )
        SynPop..$IncGrp <- cut( SynPop..$Hhincttl, breaks=IncBreaks., labels=Ig,
            include.lowest=TRUE )
  
        # Calculate total population and households
        Pop <- sum( colSums( SynPop..[ , 1:6 ] ) )
        Hhslds <- nrow( SynPop.. )
  
        # Calculate the urban rural population proportions
        if( yr <= BaseYear) {
            UrbRurPop.Dt <- UrbRurPopProp.CoDt[co,] * Pop
        } else {
            PopGrowth <- Pop - sum( BasePop.CoDt[ co, ] )
            PopGrowth.Dt <- PopGrowth * UrbRurGrowthSplit.CoDt[ co, ]
            UrbRurPop.Dt <- BasePop.CoDt[ co, ] + PopGrowth.Dt
        }
  
        # Round population to whole number and correct rounding error
        UrbRurPop.Dt <- round( UrbRurPop.Dt )
        if( sum( UrbRurPop.Dt ) != Pop ) {
            PopDiff <- Pop - sum( UrbRurPop.Dt )
            Sign <- sign( PopDiff )
            Probs.Dt <- UrbRurPop.Dt / sum( UrbRurPop.Dt )
            PopDiff. <- sample( Dt, abs( PopDiff ), replace = TRUE, prob = Probs.Dt )
            PopDiff.Dt <- Sign * table( PopDiff. )[ Dt ]
            names( PopDiff.Dt ) <- Dt
            PopDiff.Dt[ is.na( PopDiff.Dt ) ] <- 0
            UrbRurPop.Dt <- UrbRurPop.Dt + PopDiff.Dt
        }
        if( any( UrbRurPop.Dt < 0 ) ) {
            PopDiff <- sum( UrbRurPop.Dt[ UrbRurPop.Dt < 0 ] )
            UrbRurPop.Dt[ UrbRurPop.Dt < 0 ] <- 0
            Probs.Dt <- UrbRurPop.Dt / sum( UrbRurPop.Dt )
            PopDiff. <- sample( Dt, abs( PopDiff ), replace = TRUE, prob = Probs.Dt )
            PopDiff.Dt <- table( PopDiff. )[ Dt ]
            names( PopDiff.Dt ) <- Dt
            PopDiff.Dt[ is.na( PopDiff.Dt ) ] <- 0
            UrbRurPop.Dt <- UrbRurPop.Dt - PopDiff.Dt
        }
  
        # Add to population matrix in the global environment
        Pop.CoDt[ co, ] <<- UrbRurPop.Dt
  
        # Calculate the urban growth boundary area
        UgbAreas.Ut <- BaseUgbAreas.CoUt[ co, ]
        if( yr > BaseYear ) {
            PopGrowthRate.Dt <- PopGrowth.Dt / BasePop.CoDt[ co, ]
            UgbAreas.Ut <- ( 1 + PopGrowthRate.Dt[Ut] * UgbAreaGrowthRates.CoUt[co,] ) *
                UgbAreas.Ut
        }
  
        # Calculate urban density
        HasMetro <- UrbRurPop.Dt[ "Metropolitan" ] != 0
        TownDen <- UrbRurPop.Dt[ "Town" ] / UgbAreas.Ut[ "Town" ]
        if( HasMetro ) {
            MetroDen <- UrbRurPop.Dt[ "Metropolitan" ] / UgbAreas.Ut[ "Metropolitan" ]
        }
  
        # Assign development types to households
        SynPop..$DevType <- sample( Dt, nrow( SynPop.. ), replace=TRUE,
            prob = UrbRurPop.Dt / sum( UrbRurPop.Dt ) )
  
        # Calculate density distribution for metropolitan areas
        if( HasMetro ) {
            ma <- CountyGroups..[ co, "Msa" ]
        	UrbProp <- UrbanTypeProp.YrMa[ yr, ma ]
            DenUrbResults_ <- predictDensityUrban( MetroDen, UbzDenModel_, UrbProp=UrbProp )
        }
  
  			# Calculate average rural density
  			AveRuralDen <- AveRuralDen.Co[ co ]
  			if( yr > BaseYear)  {
  				if( PopGrowth.Dt[ "Rural" ] > 0 ) {
  					RuralPop <- UrbRurPop.Dt[ "Rural" ]
  					BaseRuralPop <- BasePop.CoDt[ co, "Rural" ]
  					RuralPopGrowth <- RuralPop - BaseRuralPop
  					AveRuralDen <- ( AveRuralDen * BaseRuralPop / RuralPop ) +
  					( 120 * RuralPopGrowth / RuralPop )
  				}
  			}
  	
        # Assign density and urban values to households
        Den. <- Urb. <- numeric( nrow( SynPop.. ) )
        Den.[ SynPop..$DevType == "Town" ] <- TownDen
        Den.[ SynPop..$DevType == "Rural" ] <- AveRuralDen
        if( HasMetro ) {
  				NumMetroHh <- sum( SynPop..$DevType == "Metropolitan" )
  				MetroDen. <- sample( DenUrbResults_$DenValues., NumMetroHh,
  			     	replace=TRUE, prob=DenUrbResults_$DenProbs. )
                 	Den.[ SynPop..$DevType == "Metropolitan" ] <- MetroDen.
  				UrbProb. <- DenUrbResults_$UrbanProbs.[ match( MetroDen.,
  					DenUrbResults_$DenValues. ) ]
  				MetroUrb. <- sapply( UrbProb., function(x) {
  			     	sample( c( 1, 0 ), 1, prob=c( x, 1-x ) )
  			     	} )
  				Urb.[ SynPop..$DevType == "Metropolitan" ] <- MetroUrb.
  			}
        SynPop..$Htppopdn <- Den.
        SynPop..$Urban <- Urb.
  
        # Calculate the natural log of density
        SynPop..$LogDen <- log( SynPop..$Htppopdn )
             
  			# Calculate driving age population
  			SynPop..$DrvAgePop <- rowSums( SynPop..[ , Ap[-1] ] )
  
  			# Create a variable identifying driver population levels
  			DrvLevels. <- c( 0, 1, 2, max( SynPop..$DrvAgePop ) )
  			SynPop..$DrvLevels <- as.character( cut( SynPop..$DrvAgePop, breaks=DrvLevels.,
  				labels=c( "Drv1", "Drv2", "Drv3Plus" ) ) )
  
  			# Identify households having only elderly persons
  			SynPop..$OnlyElderly <- as.numeric( SynPop..$DrvAgePop == SynPop..$Age65Plus )
  
        # Sum income by development type
        Inc.Dt <- tapply( SynPop..$Hhincttl, SynPop..$DevType, sum )
        Inc.CoDt[ co, names( Inc.Dt ) ] <<- Inc.Dt
           
        # Save the outputs
        Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
        save( SynPop.., file=Filename, compress=TRUE )
  
      } )
  
    }
  
  	print( StartTime )
  	print( Sys.time() )
  	rm( SynPop_ )
  	gc()

  #Step 1d: Save the income and population tabulations
  #===================================================
   
    # Save the income tabulation
    Filename <- paste( RunYearDir, "/Inc.CoDt.RData", sep="" )
    save( Inc.CoDt, file=Filename )
    rm( Filename )
      
    #Save the population tabulation & calculate the change in population if not the base year
    Filename <- paste( RunYearDir, "/Pop.CoDt.RData", sep="" )
    save( Pop.CoDt, file=Filename )
    if( yr == BaseYear ) {
      BasePop.CoDt <- Pop.CoDt
    }
    if( yr != BaseYear ) {
      PopChangeRatio.CoDt <- Pop.CoDt / BasePop.CoDt
      PopChangeRatio.CoDt[ is.na( PopChangeRatio.CoDt ) ] <- 0
    }
  
  #Step 1e: Calculate freeway, arterial and transit supply by metropolitan area
  #============================================================================
  
    StartTime <- Sys.time()
    print( "Calculating freeway and transit supply" )
  
    # Wrap in local function to reduce clutter and potential for conflict in global environment
    local( {
   
      # Initialize objects to store results
      FwyLnMiCap.Ma <- numeric( length( Ma ) )
      names( FwyLnMiCap.Ma ) <- Ma
      ArtLnMiCap.Ma <- numeric( length( Ma ) )
      names( ArtLnMiCap.Ma ) <- Ma
      TranRevMiCap.Ma <- numeric( length( Ma ) )
      names( TranRevMiCap.Ma ) <- Ma
      BusRevMi.Ma <- numeric( length( Ma ) )
      names( BusRevMi.Ma ) <- Ma
      RailRevMi.Ma <- numeric( length( Ma ) )
      names( RailRevMi.Ma ) <- Ma
   
      # Iterate through metropolitan areas and do the calculations
      for( ma in Ma ) {
  
  			# Select the counties in the metropolitan area
  			Mc <- rownames( CountyGroups.. )[ CountyGroups..$Msa %in% ma ]
  
  			# Calculate the metropolitan population growth proportion
  			if( yr == BaseYear ) {
  				MetroPop <- sum( BasePop.CoDt[ Mc, "Metropolitan" ] )
  			} else {
  				MetroPop <- sum( Pop.CoDt[ Mc, "Metropolitan" ] )
  				BaseMetroPop <- sum( BasePop.CoDt[ Mc, "Metropolitan" ] )
  				MetroPopChange <- ( MetroPop / BaseMetroPop ) - 1
  				if( MetroPopChange < 0 ) MetroPopChange <- 0
  			}
  
  			# Calculate per capita freeway lane miles
  			if( yr == BaseYear ) {
  				FwyLnMiCap.Ma[ ma ] <- 1000 * BaseFwyLnMi.Ma[ ma ] / MetroPop
  			} else {
  				FwyLnMiGrowth <- MetroPopChange * FreewayGrowth.Ma[ ma ] *
  				BaseFwyLnMi.Ma[ ma ]
  				FwyLnMi <- BaseFwyLnMi.Ma[ ma ] + FwyLnMiGrowth
  				FwyLnMiCap.Ma[ ma ] <- 1000 * FwyLnMi / MetroPop
  			}
  
  	    # Calculate per capita arterial lane miles
        if( yr == BaseYear ) {
            ArtLnMiCap.Ma[ ma ] <- 1000 * BaseArtLnMi.Ma[ ma ] / MetroPop
        } else {
            ArtLnMiGrowth <- MetroPopChange * ArterialGrowth.Ma[ ma ] *
                BaseArtLnMi.Ma[ ma ]
            ArtLnMi <- BaseArtLnMi.Ma[ ma ] + ArtLnMiGrowth
            ArtLnMiCap.Ma[ ma ] <- 1000 * ArtLnMi / MetroPop
        }
  
        # Calculate per capita transit revenue miles
        if( yr == BaseYear ) {
          TranRevMiCap.Ma[ ma ] <- BaseTranRevMi..[ ma, "Total" ]
          BusRevMi.Ma[ ma ] <- BaseTranRevMi..[ ma, "Bus" ] *
              MetroPop
          RailRevMi.Ma[ ma ] <- BaseTranRevMi..[ ma, "Rail" ] *
              MetroPop
        } else {
          BaseTranRevMiCap <- BaseTranRevMi..[ ma, "Total" ]
          TranRevMiGrowthFactor <- TransitParm_Va..$RevMiCapGrowth[ ma, yr ]
  		    TranRevMiCap <- BaseTranRevMiCap * TranRevMiGrowthFactor
          TranRevMiCap.Ma[ ma ] <- TranRevMiCap
          RailPropFactor <- TransitParm_Va..$PctElectric[ ma, yr ] / 100
  				BusPropFactor <- 1 - RailPropFactor
  				RailRevMi.Ma[ ma ] <- TranRevMiCap * RailPropFactor * MetroPop
  				BusRevMi.Ma[ ma ] <- TranRevMiCap * BusPropFactor * MetroPop
        }
        
      }
   
      # Assign results to the global environment
      FwyLnMiCap.Ma <<- FwyLnMiCap.Ma
      ArtLnMiCap.Ma <<- ArtLnMiCap.Ma
      TranRevMiCap.Ma <<- TranRevMiCap.Ma
      BusRevMi.Ma <<- BusRevMi.Ma
      RailRevMi.Ma <<- RailRevMi.Ma
   
    } )
  
  	# Save the results
  	#-----------------
  	Filename <- paste( RunYearDir, "/FwyLnMiCap.Ma.RData", sep="" )
  	save( FwyLnMiCap.Ma, file=Filename )
  	rm( Filename )
  	Filename <- paste( RunYearDir, "/ArtLnMiCap.Ma.RData", sep="" )
  	save( ArtLnMiCap.Ma, file=Filename )
  	rm( Filename )
  	Filename <- paste( RunYearDir, "/TranRevMiCap.Ma.RData", sep="" )
  	save( TranRevMiCap.Ma, file=Filename )
  	rm( Filename )
  	Filename <- paste( RunYearDir, "/BusRevMi.Ma.RData", sep="" )
  	save( BusRevMi.Ma, file=Filename )
  	rm( Filename )
  	Filename <- paste( RunYearDir, "/RailRevMi.Ma.RData", sep="" )
  	save( RailRevMi.Ma, file=Filename )
  	rm( Filename )
  
  	print( StartTime )
  	print( Sys.time() )
  
  #Step 1f: Add the freeway and transit supply data to the SynPop..
  #================================================================
   
  	StartTime <- Sys.time()
  	print( "Adding freeway and transit supply to synthetic households" )
  
  	for( co in Co ) {
  	
  		print(co)
  
  		local( {
          
  			# Load county file
  			Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
  			SynPop.. <- assignLoad( Filename )
                      
  			# Make calculations
  			SynPop..$Fwylnmicap <- 0
  			SynPop..$Fwylnmicap[ SynPop..$DevType == "Metropolitan" ] <-
  			FwyLnMiCap.Ma[ CountyGroups..[ co, "Msa" ] ]
  			SynPop..$Tranmilescap <- 0
  			SynPop..$Tranmilescap[ SynPop..$DevType == "Metropolitan" ] <-
  			TranRevMiCap.Ma[ CountyGroups..[ co, "Msa" ] ]
  
  			# Save results
  			save( SynPop.., file=Filename, compress=TRUE )
  
  			rm( SynPop.. )
  			gc()
  		          
  		} )
          
  	}
  
  	print( StartTime )
  	print( Sys.time() )

#=================================================================
#STEP 2: SIMULATE HOUSEHOLD TRAVEL CHARACTERISTICS FOR EACH COUNTY 
#=================================================================

	StartTime <- Sys.time()
	print( "Simulation of household travel characteristics" )

	# Iterate through counties
	#=========================
	for( co in Co ) {

		print( co )
		                      
		# Load county file
		Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
		SynPop.. <- assignLoad( Filename )
			
		# Identify metropolitan area
		MetroArea <- CountyGroups..[ co, "Msa" ]
		IsMetro. <- SynPop..$DevType == "Metropolitan"

		#Step 2a: Identify households affected by travel demand management or vehicle o&m programs
		#=========================================================================================

			# Identify PAYD households
			#-------------------------
			SynPop..$Payd <- idPayd( SynPop.., Payd..Yr[ yr, "Proportion" ] )

			# Identify ECO and IMP households
			#--------------------------------
			SynPop..$NumEco <- 0
			ModelVar. <- c( "DrvAgePop", "Houseid" )
			if( any( IsMetro. ) ){
				SynPop..$NumEco[ IsMetro. ] <- idEcoWorkers( SynPop..[ IsMetro., ModelVar. ],
				PropWrkEco=TdmParm_Va..$PropWrkEco[ MetroArea, yr ] )
			}
			rm( ModelVar. )
			SynPop..$ImpHh <- 0
			ModelVar. <- c( "Htppopdn", "Urban", "Houseid" )
			if( any( IsMetro. ) ){
				ImpHh_ <- idImpHouseholds( SynPop..[ IsMetro., ],
				ImpPropGoal=TdmParm_Va..$ImpPropGoal[ MetroArea, yr ] )
				SynPop..$ImpHh[ IsMetro. ] <- ImpHh_$ImpHh
				rm( ImpHh_ )
			}
			rm( ModelVar. )

			# Identify eco-driver and low rolling-resistance tire households
			#---------------------------------------------------------------
			# Note: reason for form of 1st argument is to pass a data frame with
			# minimal size to the functions. 
			ModelVar. <- "Houseid"
			TempInputData.. <- data.frame(SynPop..[ ,"Houseid" ] )
			SynPop..$IsEcoDriver <- idEcoDriverHh( TempInputData.., 
				EcoTire..Yr[ yr, "EcoDrvProp" ] )
			SynPop..$IsLowRollTire <- idLowRollTire( TempInputData.., 
				EcoTire..Yr[ yr, "LowRollProp" ] )
			rm( ModelVar., TempInputData.. )

		#Step 2b: Calculate vehicle ownership and adjust for carsharing
		#==============================================================

			# Calculate initial vehicle ownership
			#------------------------------------
			# Initialize Hhvehcnt and VehPerDrvAgePop variables
			SynPop..$Hhvehcnt <- 0
			SynPop..$VehPerDrvAgePop <- 0
			# Predict ownership for metropolitan households if any exist
			if( any( IsMetro. ) ) {
			  ModelVar. <- c( "Hhincttl", "Htppopdn", "Tranmilescap", "Urban", 
					"Fwylnmicap", "OnlyElderly", "DrvLevels", "DrvAgePop" )  
				MetroVehOwn_ <- predictVehOwn( SynPop..[ IsMetro., ModelVar. ],
					Model_=VehicleOwnModels_, Type="Metro", adjustments = VehOwnAdjustments)
				rm( ModelVar. )
			}
			# Predict ownership for nonmetropolitan households if any exist
			if( any( !IsMetro. ) ) {
				ModelVar. <- c( "Hhincttl", "Htppopdn", "OnlyElderly", "DrvLevels", "DrvAgePop" )  
				NonMetroVehOwn_ <- predictVehOwn( SynPop..[ !IsMetro., ModelVar. ],
					Model_=VehicleOwnModels_, Type="NonMetro", adjustments = VehOwnAdjustments)
				rm( ModelVar. )
			}
			# Assign values to SynPop.. and return the result
			if( any( IsMetro. ) ) {
				SynPop..$Hhvehcnt[ IsMetro. ] <- MetroVehOwn_$NumVeh
				SynPop..$VehPerDrvAgePop[ IsMetro. ] <- MetroVehOwn_$VehRatio
			}
			if( any( !IsMetro. ) ) {
				SynPop..$Hhvehcnt[ !IsMetro. ] <- NonMetroVehOwn_$NumVeh
				SynPop..$VehPerDrvAgePop[ !IsMetro. ] <- NonMetroVehOwn_$VehRatio
			}
			# Clean up
			if( exists( "MetroVehOwn_" ) ) rm( MetroVehOwn_ )
			if( exists( "NonMetroVehOwn_" ) ) rm( NonMetroVehOwn_ )
		
			# Identify carshare households
			#-----------------------------
			SynPop..$Carshare <- 0
			ModelVar. <- c( "Hhvehcnt", "Hhsize", "Age65Plus", "Htppopdn", "Houseid" )
			if( any( IsMetro. ) ) {              
        SynPop..$Carshare[ IsMetro. ] <- idCarshareHh( SynPop..[ IsMetro., ModelVar. ],
				CarshareRates.=c( MedDen=CarshareParm_Va..$MedDenRate[ MetroArea, yr ],
				HighDen=CarshareParm_Va..$HighDenRate[ MetroArea, yr ] ) )
			}
			rm( ModelVar. )
          
			# Adjust vehicle ownership to account for carsharing
			#---------------------------------------------------
			if( any( IsMetro. ) ) {
				SynPop.. <- adjCarshareOwn( SynPop.., OneCarProb.=c( C0=0.66, C1=0.34 ),
				TwoCarProb.=c( C0=0.17, C1=0.56, C2=0.27 ),
				ThreeCarProb.=c( C0=0.15, C1=0.21, C2=0.22, C3=0.42 ) )
			}

		#Step 2c: 1st DVMT calculation (no adjustment for costs)
		#=======================================================

			# Calculate the average DVMT
			#---------------------------
			ModelVar. <- c( "Hhincttl", "Htppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", "DrvAgePop", "Hhsize", 
				"Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus", "Urban", "BaseCostPerMi",
				"FutrCostPerMi" )
			# Assume a base and future cost of 4 cents per mile
			# so that budget constraints don't impinge on the amount of vehicle travel
			SynPop..$BaseCostPerMi <- 4 / 100
			SynPop..$FutrCostPerMi <- 4 / 100
			SynPop..$Dvmt <- 0
            
      SynPop..$Census_rNortheast = Census_rNortheast
      SynPop..$Census_rWest = Census_rWest
      SynPop..$Census_rSouth = Census_rSouth
      SynPop..$Census_rMidwest = Census_rMidwest

      vmtadj = CountyGroups..[co,]$VmtAdjustment
            
			if( any( IsMetro. ) ) {
				SynPop..$Dvmt[ IsMetro. ] <- calcAdjAveDvmt( SynPop..[ IsMetro., ModelVar. ],
				DvmtLmModels_, "Metro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator,
				TrnstnProp=1, VmtAdj = vmtadj )[[1]]
			}
			if( any( !IsMetro. ) ) {
				SynPop..$Dvmt[ !IsMetro. ] <- calcAdjAveDvmt( SynPop..[ !IsMetro., ModelVar. ],
				DvmtLmModels_, "NonMetro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator,
				TrnstnProp=1, VmtAdj = vmtadj )[[1]]
			}

		#Step 2d: Calculate non-price TDM and light-weight vehicle DVMT adjustment factors
		#=================================================================================

			# Calculate the TDM adjustment factor
			#------------------------------------
			TdmAdjDvmt.Hh <- SynPop..$Dvmt
			ModelVar. <- c( "Dvmt", "NumEco", "ImpHh" )
			if( any( IsMetro. ) ) {
				TdmAdjDvmt.Hh[ IsMetro. ] <- adjDvmtEcoImp( SynPop..[ IsMetro., ModelVar. ],
				EcoReduction=TdmParm_Va..$EcoReduction[ MetroArea, yr ],
				ImpReduction=TdmParm_Va..$ImpReduction[ MetroArea, yr ] )
			}
			TdmAdjFactor.Hh <- TdmAdjDvmt.Hh / SynPop..$Dvmt
			TdmAdjFactor.Hh[ SynPop..$Dvmt == 0 ] <- 1

			# Calculate the light vehicle adjustment factor
			#----------------------------------------------
			# Predict light vehicle ownership
			LtVehOwn.Hh <- rep( 0, nrow( SynPop.. ) )
			SynPop..$LogDen <- log( SynPop..$Htppopdn )                 
			ModelVar. <- c( "LogDen", "Hhsize", "Hhincttl", "Age15to19", "Age20to29", "Age30to54", 
				"Age55to64", "Age65Plus", "VehPerDrvAgePop", "DrvAgePop" ) 
			
      SynPop..$Census_rNortheast = Census_rNortheast
      SynPop..$Census_rWest = Census_rWest
      SynPop..$Census_rSouth = Census_rSouth
      SynPop..$Census_rMidwest = Census_rMidwest
            
      if( any( IsMetro. ) ) {
				LtVehOwn.Hh[ IsMetro. ] <- predictLightVehicles( SynPop..[ IsMetro., ModelVar. ],
				LtVehOwnModels_=LtVehOwnModels_, Type="Metro",
				TargetProp=LtVehParm_Va..$TargetProp[ MetroArea, yr ] )
			}
			if( any( !IsMetro. ) ) {
				LtVehOwn.Hh[ !IsMetro. ] <- predictLightVehicles( SynPop..[ !IsMetro., ModelVar. ],
				LtVehOwnModels_=LtVehOwnModels_, Type="NonMetro",
				TargetProp=LtVehParm_Va..$TargetProp[ "NonMetro", yr ] )
			}
			SynPop..$LtVehCnt <- LtVehOwn.Hh
			rm( LtVehOwn.Hh, ModelVar. )
			SynPop..$LogDen <- NULL
     	
			# Predict light vehicle DVMT
			#---------------------------
			LtVehDvmt.Hh <- SynPop..$Dvmt
			SynPop..$LogDen <- log( SynPop..$Htppopdn )
			SynPop..$LogSize <- log( SynPop..$Hhsize )
			SynPop..$LogDvmt <- log( SynPop..$Dvmt )
			ModelVar. <- c( "Hhincttl", "LogDen", "LogSize", "Urban", "LogDvmt", "Dvmt", "LtVehCnt",
				"DrvAgePop" )
			
      SynPop..$Census_rNortheast = Census_rNortheast
      SynPop..$Census_rWest = Census_rWest
      SynPop..$Census_rSouth = Census_rSouth
      SynPop..$Census_rMidwest = Census_rMidwest
            
      if( any( IsMetro. ) ) {
				LtVehDvmt.Hh[ IsMetro. ] <- calcLtVehDvmt( SynPop..[ IsMetro., ModelVar. ], 
				AveSovPropModels_, Threshold=LtVehParm_Va..$Threshold[ MetroArea, yr ],
				PropSuitable=LtVehParm_Va..$PropSuitable[ MetroArea, yr ], Sharing=FALSE )
			}
			if( any( !IsMetro. ) ) {
				LtVehDvmt.Hh[ !IsMetro. ] <- calcLtVehDvmt( SynPop..[ !IsMetro., ModelVar. ], 
				AveSovPropModels_, Threshold=LtVehParm_Va..$Threshold[ "NonMetro", yr ],
				PropSuitable=LtVehParm_Va..$PropSuitable[ "NonMetro", yr ], Sharing=FALSE )
			}
			# Calculate adjustment factor
			LtVehAdjFactor.Hh <- ( SynPop..$Dvmt - LtVehDvmt.Hh ) / SynPop..$Dvmt
			LtVehAdjFactor.Hh[ SynPop..$Dvmt == 0 ] <- 1

			# Calculate overall adjustment factor and apply to adjust DVMT
			#-------------------------------------------------------------
			TdmLtVehAdjFactor.Hh <- TdmAdjFactor.Hh * LtVehAdjFactor.Hh
			SynPop..$LtVehDvmt <- LtVehDvmt.Hh
			SynPop..$TdmLtVehAdjFactor <- TdmLtVehAdjFactor.Hh
			SynPop..$Dvmt <- SynPop..$Dvmt * TdmLtVehAdjFactor.Hh
			rm( LtVehDvmt.Hh, TdmAdjFactor.Hh, LtVehAdjFactor.Hh, TdmLtVehAdjFactor.Hh, ModelVar.,
				TdmAdjDvmt.Hh )
			SynPop..$LogDen <- NULL
			SynPop..$LogSize <- NULL
			SynPop..$LogDvmt <- NULL

 		#Step 2e: Calculate the 95th percentile and maximum DVMT from the adjusted DVMT
 		#==============================================================================

			SynPop..$MaxDvmt <- 0
			SynPop..$Dvmt95 <- 0
          
      SynPop..$Census_rNortheast = Census_rNortheast
      SynPop..$Census_rWest = Census_rWest
      SynPop..$Census_rSouth = Census_rSouth
      SynPop..$Census_rMidwest = Census_rMidwest
          
		  if( any( IsMetro. ) ) {
				MetroMax95th.2d <- predictMaxDvmt( SynPop..[ IsMetro., c( "Dvmt", "MaxDvmt", "Dvmt95" ) ],
					DvmtLmModels_, "Metro" )
				SynPop..$MaxDvmt[IsMetro.] <- MetroMax95th.2d[,1]
				SynPop..$Dvmt95[IsMetro.] <- MetroMax95th.2d[,2]
				rm( MetroMax95th.2d )
			}
			if( any( !IsMetro. ) ) {
				NonMetroMax95th.2d <- predictMaxDvmt( SynPop..[ !IsMetro., c( "Dvmt", "MaxDvmt", "Dvmt95" ) ],
				DvmtLmModels_, "NonMetro" )
				SynPop..$MaxDvmt[!IsMetro.] <- NonMetroMax95th.2d[,1]
				SynPop..$Dvmt95[!IsMetro.] <- NonMetroMax95th.2d[,2]
				rm( NonMetroMax95th.2d )
			}
			gc()

		#Step 2f: Calculate Walk Trips
		#=============================
		# This is a performance measure. It does not affect DVMT calculations.

		  ModelVar. <- c( "Hhsize", "Age0to14", "Age15to19", "Age20to29", "Age30to54",
			"Age55to64", "Age65Plus", "VehPerDrvAgePop", "Htppopdn", "Urban", "Hhincttl" )
     	SynPop..$AveWalkTrips <- calcWalkTrips( SynPop..[ , ModelVar. ], WalkModel_ )		

			# Remove variables from SynPop.. not needed
			#------------------------------------------
			SynPop..$PowPerCapInc <- NULL
			SynPop..$DrvLevels <- NULL
			SynPop..$OnlyElderly <- NULL
			SynPop..$NumEco <- NULL
			SynPop..$ImpHh <- NULL
			SynPop..$LtVehCnt <- NULL

			# Save results
			#-------------			
			Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
			save( SynPop.., file=Filename, compress=TRUE )
			rm( SynPop.. )
			gc()

		# End loop through counties
		gc()
	}
	print( StartTime )
	print( Sys.time() )

#==================================================================
#STEP 3: SIMULATE HOUSEHOLD VEHICLE CHARACTERISTICS FOR EACH COUNTY 
#==================================================================

	StartTime <- Sys.time()
	print( "Simulation of household vehicle characteristics" )

	# Iterate through counties
	#=========================
        
	for( co in Co ) {

		print( co )
		                      
		# Load county file
		Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
		SynPop.. <- assignLoad( Filename )
			
		# Identify metropolitan area
		MetroArea <- CountyGroups..[ co, "Msa" ]
		IsMetro. <- SynPop..$DevType == "Metropolitan"


		#Step 3a: Calculate vehicle types, ages, initial fuel economy, and assign vehicle DVMT
		#=====================================================================================

		# Predict light truck ownership and vehicle ages
		#-----------------------------------------------
		# Apply vehicle type model
		ModelVar. <- c( "Hhincttl", "Htppopdn", "Urban", "Hhvehcnt", "Hhsize" ) 
		SynPop..$VehType <- predictLtTruckOwn( SynPop..[ , ModelVar. ], Model_=LtTruckModels_[[Census_r]],
			TruckProp=LtTruckProp.CoYr[co,yr] )
		rm( ModelVar. )
		# Apply vehicle age model
		ModelVar. <- c( "IncGrp", "Hhvehcnt", "VehType" ) 
		
          
          VehTypeAgeResults_ <- calcVehicleAges( SynPop..[ , ModelVar. ], VProp_=VehProp_,
			AdjRatio=AgeAdj.YrTy[yr,] )
		rm( ModelVar. )
		# Add type and age model results to the TestHh..
		SynPop..$VehType[ SynPop..$Hhvehcnt == 0 ] <- NA
		SynPop..$VehAge <- VehTypeAgeResults_$VehAge
		SynPop..$VehAge[ SynPop..$Hhvehcnt == 0 ] <- NA
		rm( VehTypeAgeResults_ )
		gc()

		# Assign initial fuel economy and DVMT to vehicles
		#-------------------------------------------------
		# Assign fuel economy to vehicles
		HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
		SynPop..$VehMpg <- NA
		ModelVar. <- c( "VehType", "VehAge", "Hhvehcnt" )
		SynPop..$VehMpg[ HasVeh.Hh ] <-assignFuelEconomy( SynPop..[ HasVeh.Hh, ModelVar. ],
			AutoLtTrkMpg..Yr, CurrYear=yr )
		rm( ModelVar. )
		# Assign vehicle mileage proportions to household vehicles
		SynPop..$DvmtProp <- NA
		ModelVar. <- c( "Hhvehcnt", "Houseid" ) 
		SynPop..$DvmtProp[ HasVeh.Hh ] <- apportionDvmt( SynPop..[ HasVeh.Hh, ],
			DP_=DvmtProp_ )
		rm( ModelVar. )
		# Assign vehicle mileage to household vehicles
		SynPop..$VehDvmt <- calcVehDvmt( SynPop..$Dvmt, SynPop..$DvmtProp )
		gc()

		#Step 3b: Identify HEVs & PHEVs
		#==============================

		# Apply HEV/PHEV model
		HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
		ModelVar. <- c( "Houseid", "Hhvehcnt", "VehType", "VehAge", "VehDvmt", "Carshare",
			"DevType", "Hhincttl", "Htppopdn", "Hhsize", "Age0to14", "Age65Plus",
			"Tranmilescap", "Urban", "VehMpg" )
		PhevResults_ <- assignPhev( SynPop..[ HasVeh.Hh, ModelVar. ],
			PhevRangeProp..Yr=PhevRangeProp..Yr, CurrYear=yr,
			PhevPropModel_=PhevMilePropModel_, HevMpgProp..Yr=HevMpgProp..Yr, 
			OptimProp=OptimProp.Yr[yr] )
		rm( ModelVar. )

		# Update SynPop.. data
		SynPop..$VehDvmt[ HasVeh.Hh ] <- PhevResults_$VehDvmt_
		SynPop..$DvmtProp[ HasVeh.Hh ] <- PhevResults_$DvmtProp_
		SynPop..$EvVehDvmt <- NA
		SynPop..$EvVehDvmt[ HasVeh.Hh ] <- PhevResults_$EvVehDvmt_
		SynPop..$HcVehDvmt <- NA
		SynPop..$HcVehDvmt[ HasVeh.Hh ] <- PhevResults_$HcVehDvmt_
		SynPop..$VehMpg[ HasVeh.Hh ] <- PhevResults_$VehMpg_
		SynPop..$VehMpkwh <- NA
		SynPop..$VehMpkwh[ HasVeh.Hh ] <- PhevResults_$VehMpkwh_
		SynPop..$Powertrain <- NA
		SynPop..$Powertrain[ HasVeh.Hh ] <- PhevResults_$Powertrain_
		rm( PhevResults_, HasVeh.Hh )
		gc()

		#Step 3c: Identify EVs
		#=====================

		# Apply EV model	
		ModelVar. <- c( "Houseid", "Hhvehcnt", "VehType", "VehAge", "VehDvmt", "Dvmt95", 
			"DvmtProp", "Powertrain", "VehMpg", "VehMpkwh", "EvVehDvmt", "HcVehDvmt" )
		HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
		EvResults_ <- assignEv( SynPop..[ HasVeh.Hh, ModelVar. ], EvRangeProp..Yr=EvRangeProp..Yr,
			CurrYear=yr )
		SynPop..$EvVehDvmt[ HasVeh.Hh ] <- EvResults_$EvVehDvmt_
		SynPop..$HcVehDvmt[ HasVeh.Hh ] <- EvResults_$HcVehDvmt_
		SynPop..$VehMpg[ HasVeh.Hh ] <- EvResults_$VehMpg_
		SynPop..$VehMpkwh[ HasVeh.Hh ] <- EvResults_$VehMpkwh_
		SynPop..$Powertrain[ HasVeh.Hh ] <- EvResults_$Powertrain_
		rm( EvResults_, HasVeh.Hh, ModelVar. )
		gc()
				
		Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
		save( SynPop.., file=Filename, compress=TRUE )
		rm( SynPop.. )
		gc()
		gc()

	# End loop through counties
	gc()
	}
	print( StartTime )
	print( Sys.time() )

#=====================================================
#STEP 4: EQUILIBRATE DVMT, COSTS, REVENUES, CONGESTION 
#=====================================================

	# Loop 4 times to equilibrate DVMT, travel costs, revenues and congestion
	#------------------------------------------------------------------------
	
	#Create list to store DVMT, cost totals
	Dvmt.CoDt <- Pop.CoDt * 0
	Va <- c( "Dvmt", "AdjDvmt", "CongPrice", "FuelCost", "PowerCost", "RoadUseTax", "CarbonTax", "AddedExtTax", 
		"PaydCost", "TotExtCost", "HhTotCost", "FutrCostPerMi", "VehOwnExp", "TotRoadCost" )
	CostSummary.CoVa <- array( 0, dim=c( length(Co), length(Va) ), dimnames=list(Co,Va) )
	AveCongTaxPerMi.Ma <- numeric( length(Ma) )
	names( AveCongTaxPerMi.Ma ) <- Ma
	ExtraModCost <- 0
	It <- 1:4
	VmtSurcharge.It <- numeric( length(It) )
	rm( Va ) 
			
	for( it in It ) {

		StartTime <- Sys.time()
		print( paste( "Iteration", it, "Calculate emissions and cost and adjust DVMT" ) )

		for( co in Co ) {

			print( co )
			                      
			# Load county file
			Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )  
			SynPop.. <- assignLoad( Filename )
              
			# Identify metropolitan area
			MetroArea <- CountyGroups..[ co, "Msa" ]
			IsMetro. <- SynPop..$DevType == "Metropolitan"


			#Step 4a: Calculate fuel & electricity consumption, CO2e production, & household costs
			#=====================================================================================

				#Calculate fuel & electricity consumption and CO2e production
				#------------------------------------------------------------

				# Calculate consumption and production at a household level
				ModelVar. <- c( "Hhvehcnt", "HcVehDvmt", "VehMpg", "VehType", "EvVehDvmt",
					"VehMpkwh", "Dvmt" ) 
				FuelElecCo2e_ <- calcVehFuelElecCo2( SynPop..[ , ModelVar. ], AveFuelCo2e.=AveFuelCo2e.,
					AveElectricCo2e=AveElectricCo2e.Co[co], CsharEffFactor=1 )
				SynPop..$FuelGallons <- FuelElecCo2e_$FuelGallons
				SynPop..$FuelCo2e <- FuelElecCo2e_$FuelCo2e
				SynPop..$ElecKwh <- FuelElecCo2e_$ElecKwh
				SynPop..$ElecCo2e <- FuelElecCo2e_$ElecCo2e
				rm( FuelElecCo2e_ )
				rm( ModelVar. )
				gc()

				#Apply parking model to identify parkers and calculate daily parking costs
				#-------------------------------------------------------------------------

				# Only do on 1st iteration
				if( it == 1 ) {
				
					# Calculate parking costs for households that live in metropolitan areas
					SynPop..$DailyPkgCost <- 0
					SynPop..$CashOutIncAdj <- 0
					ModelVar. <- c( "DrvAgePop", "Houseid", "Dvmt", "Hhvehcnt" ) 
					if( any( IsMetro. ) ) {
						Parkers_ <- idPayingParkers( SynPop..[ IsMetro., ModelVar. ],
							PropWrkPkg=PkgParm_Va..$PropWrkPkg[ MetroArea, yr ],
							PropWrkChrgd=PkgParm_Va..$PropWrkChrgd[ MetroArea, yr ],
							PropCashOut=PkgParm_Va..$PropCashOut[ MetroArea, yr ],
							PropOthChrgd=PkgParm_Va..$PropOthChrgd[ MetroArea, yr ],
							PkgCost=PkgParm_Va..$PkgCost[ MetroArea, yr ],
							PropWrkTrav=0.22, WrkDaysPerYear=260 )
						PkgCosts_ <- calcParkCostAdj( SynPop..[ IsMetro., ModelVar. ], Parkers_ )
						SynPop..$DailyPkgCost[ IsMetro. ] <- PkgCosts_$DailyPkgCost
						SynPop..$Hhincttl[ IsMetro. ] <- SynPop..$Hhincttl[ IsMetro. ] + PkgCosts_$CashOutIncAdj
						rm( Parkers_, PkgCosts_ )
					}
					rm( ModelVar. )
					gc()
					
				}
			
				#Calculate vehicle depreciation expenses
				#---------------------------------------
			
				# Only do on 1st iteration
				if( it == 1 ) {
				
					SynPop..$DepExp <- 0
					HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
					ModelVar. <- c( "Houseid", "Hhvehcnt", "VehType", "VehAge" )
					SynPop..$DepExp[ HasVeh.Hh ] <- calcVehDepreciationExp( SynPop..[ HasVeh.Hh, ModelVar. ] )
					rm( HasVeh.Hh, ModelVar. )
					gc()
				
				}

				#Calculate household costs
				#-------------------------

				# Identify congestion price for metropolitan area if any
				if( !is.na( MetroArea ) ) {
					CongPrice <- AveCongTaxPerMi.Ma[ MetroArea ]
				} else {
					CongPrice <- 0
				}
				# Identify the VmtSurcharge calculated to balance costs and revenues
				if( it == 1 ) {
					VmtSurcharge <- 0
				} else {
					VmtSurcharge <- VmtSurcharge.It[ it - 1 ]   # VmtSurcharge is value calculated in previous iteration
				}
				
				# Run model  
				ModelVar. <- c( "Dvmt", "FuelGallons", "FuelCo2e", "ElecCo2e", "ElecKwh", "DevType", "Payd", 
					"DailyPkgCost", "Hhvehcnt", "DepExp", "Hhincttl" )
				Costs_ <- calcCosts( Data..=SynPop..[ , ModelVar. ], 
					Costs.=Costs.YrCs[ yr, ], 
					PaydRate=Payd..Yr[ yr, "RatePerMile" ],
					CongPrice=CongPrice,
					VmtSurcharge=VmtSurcharge * Costs.YrCs[yr,"VmtSurchargeMultiplier"], 
					ExtraModCost=ExtraModCost )
				rm( VmtSurcharge )	
				# Add selected cost data to household records
				SynPop..$FutrCostPerMi <- Costs_$FutrCostPerMi
				SynPop..$TotExtCost <- Costs_$TotExtCost
				SynPop..$HhTotCost <- Costs_$HhTotCost
				SynPop..$VehOwnExp <- Costs_$VehOwnExp
				
				# Add sums to DVMT and cost summary
				Dvmt.CoDt[co,] <- tapply( SynPop..$Dvmt, SynPop..$DevType, sum )[Dt]
				Dvmt.CoDt[ is.na( Dvmt.CoDt ) ] <- 0
				CostSummary.CoVa[ co, "Dvmt" ] <- sum( Dvmt.CoDt[co,] )
				CostSummary.CoVa[ co, "FuelCost" ] <- sum( Costs_$FuelCost )  
				CostSummary.CoVa[ co, "PowerCost" ] <- sum( Costs_$PowerCost ) 
				CostSummary.CoVa[ co, "RoadUseTax" ] <- sum( Costs_$RoadUseTax )
				CostSummary.CoVa[ co, "CarbonTax" ] <- sum( Costs_$CarbonTax )
				CostSummary.CoVa[ co, "AddedExtTax" ] <- sum( Costs_$AddedExtTax )
				CostSummary.CoVa[ co, "PaydCost" ] <- sum( Costs_$PaydCost )
				CostSummary.CoVa[ co, "TotExtCost" ] <- sum( Costs_$TotExtCost )
				CostSummary.CoVa[ co, "HhTotCost" ] <- sum( Costs_$HhTotCost )
				CostSummary.CoVa[ co, "FutrCostPerMi" ] <- sum( Costs_$FutrCostPerMi )
				CostSummary.CoVa[ co, "VehOwnExp" ] <- sum( Costs_$VehOwnExp )
				CostSummary.CoVa[ co, "TotRoadCost" ] <- sum( Costs_$TotRoadCost )
				rm( Costs_, ModelVar. )
				gc()


			#Step 4b: Calculate DVMT with new costs and reallocate to vehicles
			#=================================================================

				# Recalculate DVMT
				#-----------------
				PrevDvmt.Hh <- SynPop..$Dvmt
				ModelVar. <- c( "Hhincttl", "Htppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", "DrvAgePop", 
					"Hhsize", "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus", 
					"Urban", "BaseCostPerMi", "FutrCostPerMi" )
				SynPop..$Census_rNortheast = Census_rNortheast
        SynPop..$Census_rWest = Census_rWest
        SynPop..$Census_rSouth = Census_rSouth
        SynPop..$Census_rMidwest = Census_rMidwest
        
        vmtadj = CountyGroups..[co,]$VmtAdjustment
              
        if( any( IsMetro. ) ) {
				  SynPop..$Dvmt[ IsMetro. ] <- calcAdjAveDvmt( SynPop..[ IsMetro., ModelVar. ], DvmtLmModels_,
					"Metro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator, TrnstnProp=1, VmtAdj = vmtadj  )[[1]]
		    }
				if( any( !IsMetro. ) ) {
					SynPop..$Dvmt[ !IsMetro. ] <- calcAdjAveDvmt( SynPop..[ !IsMetro., ModelVar. ], 
					DvmtLmModels_, "NonMetro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator, 
						TrnstnProp=1, VmtAdj = vmtadj  )[[1]]
				}

				# Adjust for TDM and calculate 95th percentile and maximum DVMT
				#--------------------------------------------------------------
				SynPop..$Dvmt <- SynPop..$Dvmt * SynPop..$TdmLtVehAdjFactor
				SynPop..$MaxDvmt <- 0
				SynPop..$Dvmt95 <- 0
              
        SynPop..$Census_rNortheast = Census_rNortheast
        SynPop..$Census_rWest = Census_rWest
        SynPop..$Census_rSouth = Census_rSouth
        SynPop..$Census_rMidwest = Census_rMidwest
              
        if( any( IsMetro. ) ) {
				  MetroMax95th.2d <- predictMaxDvmt( SynPop..[ IsMetro., c( "Dvmt", "MaxDvmt", "Dvmt95" ) ],
					DvmtLmModels_, "Metro" )
					SynPop..$MaxDvmt[IsMetro.] <- MetroMax95th.2d[,1]
					SynPop..$Dvmt95[IsMetro.] <- MetroMax95th.2d[,2]
					rm( MetroMax95th.2d )
			  }
			  if( any( !IsMetro. ) ) {
					NonMetroMax95th.2d <- predictMaxDvmt( SynPop..[ !IsMetro., c( "Dvmt", "MaxDvmt", "Dvmt95" ) ],
						DvmtLmModels_, "NonMetro" )
					SynPop..$MaxDvmt[!IsMetro.] <- NonMetroMax95th.2d[,1]
					SynPop..$Dvmt95[!IsMetro.] <- NonMetroMax95th.2d[,2]
					rm( NonMetroMax95th.2d )
				}
				gc()

				# Split adjusted DVMT among vehicles
				#-----------------------------------
				DvmtAdjFactor.Hh <- SynPop..$Dvmt / PrevDvmt.Hh
				HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
				ModelVar. <- c( "VehDvmt", "HcVehDvmt", "EvVehDvmt" )
				AdjDvmt_ <- allocateAdjDvmt( SynPop..[ HasVeh.Hh, ModelVar. ], DvmtAdjFactor.Hh[ HasVeh.Hh ] )
				SynPop..$VehDvmt[ HasVeh.Hh ] <- AdjDvmt_$VehDvmt
				SynPop..$EvVehDvmt[ HasVeh.Hh ] <- AdjDvmt_$EvVehDvmt
				SynPop..$HcVehDvmt[ HasVeh.Hh ] <- AdjDvmt_$HcVehDvmt
				rm( DvmtAdjFactor.Hh, HasVeh.Hh, ModelVar., AdjDvmt_ )
				gc()

				# Tabulate DVMT
				#--------------
				Dvmt.CoDt[ co, ] <- tapply( SynPop..$Dvmt, SynPop..$DevType, sum, na.rm=TRUE )[Dt]
				Dvmt.CoDt[ is.na( Dvmt.CoDt ) ] <- 0
				CostSummary.CoVa[ co, "AdjDvmt" ] <- sum( SynPop..$Dvmt, na.rm=TRUE )
				 
				# Save the household dataset
				#---------------------------
 				Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
				save( SynPop.., file=Filename, compress=TRUE )
				rm( SynPop.. )
				gc()
				gc()
				print( memory.size() )
   
		# End iteration through counties
		}

		# Save the tabulations of Dvmt and Costs
		#---------------------------------------
		Filename <- paste( RunYearDir, "/", "Dvmt.CoDt", ".RData", sep="" )
		save( Dvmt.CoDt, file=Filename )
		rm( Filename )
		Filename <- paste( RunYearDir, "/", "CostSummary.CoVa", ".RData", sep="" )
		save( CostSummary.CoVa, file=Filename )
          rm( Filename )
		print( StartTime )
		print( Sys.time() )


		#Step 4c: Calculate Effects of Congestion
		#========================================

		  local( {

  			# Load data summaries
  			#--------------------
  			# Load base year income
  			if( yr == BaseYear ) {
  				Filename <- paste( RunYearDir, "/Inc.CoDt.RData", sep="" )
  				Inc.CoDt <- assignLoad( Filename )
  				BaseInc.CoDt <- Inc.CoDt
  			}
  			if( yr != BaseYear ) {
  				Filename <- paste( RunYearDir, "/Inc.CoDt.RData", sep="" )
  				Inc.CoDt <- assignLoad( Filename )
  				Filename <- paste( OutputDir, "/Year", BaseYear, "/", "Inc.CoDt.RData", sep="" )
  				BaseInc.CoDt <- assignLoad( Filename )
  			}
  			# Load population
  			if( yr == BaseYear ) {
  				Filename <- paste( RunYearDir, "/Pop.CoDt.RData", sep="" )
  				Pop.CoDt <- assignLoad( Filename )
  				BasePop.CoDt <- Pop.CoDt
  			}
  			if( yr != BaseYear ) {
  				Filename <- paste( RunYearDir, "/Pop.CoDt.RData", sep="" )
  				Pop.CoDt <- assignLoad( Filename )
  				Filename <- paste( OutputDir, "/Year", BaseYear, "/", "Pop.CoDt.RData", sep="" )
  				BasePop.CoDt <- assignLoad( Filename )
  			}
  			PopChangeRatio.CoDt <- Pop.CoDt / BasePop.CoDt
  			PopChangeRatio.CoDt[ is.na( PopChangeRatio.CoDt ) ] <- 0
  			# Load metropolitan transportation summaries
  			Filename <- paste( RunYearDir, "/FwyLnMiCap.Ma.RData", sep="" )        
  			FwyLnMiCap.Ma <- assignLoad( Filename )
  			Filename <- paste( RunYearDir, "/ArtLnMiCap.Ma.RData", sep="" )
  			ArtLnMiCap.Ma <- assignLoad( Filename )
  			Filename <- paste( RunYearDir, "/TranRevMiCap.Ma.RData", sep="" )
  			TranRevMiCap.Ma <- assignLoad( Filename )
  			Filename <- paste( RunYearDir, "/BusRevMi.Ma.RData", sep="" )
  			BusRevMi.Ma <- assignLoad( Filename )
  			Filename <- paste( RunYearDir, "/RailRevMi.Ma.RData", sep="" )
  			RailRevMi.Ma <- assignLoad( Filename )
  			rm( Filename )
  			# Load DVMT
  			Filename <- paste( RunYearDir, "/", "Dvmt.CoDt", ".RData", sep="" )
  			Dvmt.CoDt <- assignLoad( Filename )
  			rm( Filename )
            
  			# Calculate bus DVMT by metropolitan area
  			#----------------------------------------
  			# Calculate bus DVMT
  			BusDvmt.Ma <- BusRevMi.Ma * TranAdjFactor / 365
  			
  			# Calculate truck VMT by metropolitan area
  			#-----------------------------------------
  			# Load truck DVMT
  			
  			Filename <- paste( RunYearDir, "/TruckDvmt.Ma.RData", sep="" )
  			TruckDvmt.Ma <- assignLoad( Filename )
  			rm( Filename )
  			
  			# Calculate light vehicle DVMT by metropolitan area
  			#--------------------------------------------------
  			# Sum household light vehicle DVMT by metropolitan area
  			CoToMa. <- CountyGroups..$Msa
  			names( CoToMa. ) <- rownames( CountyGroups.. )
  			CoToMa. <- CoToMa.[ !is.na( CoToMa. ) ]
  			HhDvmt.Ma <- tapply( Dvmt.CoDt[ names( CoToMa. ), "Metropolitan" ], CoToMa., sum )[ Ma ]   
  			# Calculate commercial service vehicle DVMT and total light vehicle DVMT
  			CommVehDvmt.CoDt <- Dvmt.CoDt * CommVmtFactor
  			CommVehDvmt.Ma <- tapply( CommVehDvmt.CoDt[ names( CoToMa. ), "Metropolitan" ], CoToMa., sum )[ Ma ]
  			# Calculate total light vehicle DVMT that is on metropolitan area roadways
  			LtVehDvmt.Ma <- ( HhDvmt.Ma + CommVehDvmt.Ma ) * LtVehDvmtFactor.Ma
  			# Clean up
  			rm( CoToMa. )
  		
  			# Calculate total DVMT by metropolitan area and type
  			#---------------------------------------------------
  			Dvmt.MaTy <- cbind( LtVeh=LtVehDvmt.Ma, Truck=rowSums(TruckDvmt.Ma), Bus=BusDvmt.Ma )
  
  			# Sum population by metropolitan area
  			#------------------------------------
  			CoToMa. <- CountyGroups..$Msa
  			names( CoToMa. ) <- rownames( CountyGroups.. )
  			CoToMa. <- CoToMa.[ !is.na( CoToMa. ) ]
  			Pop.Ma <- tapply( Pop.CoDt[ names( CoToMa. ), "Metropolitan" ], CoToMa., sum )[ Ma ]    
  			rm( CoToMa. )
  		
  			# Initialize arrays to store results
  			#-----------------------------------
  			Ty <- Abbr_$Ty
  			MpgMpkwhAdj.MaPt <- array( 0, dim=c(length(Ma),length(Pt)), dimnames=list(Ma,Pt) )
  			VehHr.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
  			AveSpeed.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
  			FfVehHr.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
  			DelayVehHr.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
  			CongVmt.ClFcTyMa <- array( 0, dim=c(length(Cl), length(Fc), length(Ty), length(Ma)), 
  				dimnames=list(Cl,Fc,Ty,Ma) )
  			AveCongTaxPerMi.Ma <- numeric( length( Ma ) )
  			names( AveCongTaxPerMi.Ma ) <- Ma
  
  			# Calculate effects of congestion on speed and emissions
  			#-------------------------------------------------------
  			for( ma in Ma ) {
  	
              	# Make an array of congestion prices
  				CongPrice.ClFc <- array( 0, dim=c( length(Cl), length(Fc) ), dimnames=list( Cl, Fc ) )
  				CongPrice.ClFc[ "Sev", "Fwy" ] <- CongPriceParm_Va..$FwySev[ ma, yr ]
  				CongPrice.ClFc[ "Ext", "Fwy" ] <- CongPriceParm_Va..$FwyExt[ ma, yr ]
  				CongPrice.ClFc[ "Sev", "Art" ] <- CongPriceParm_Va..$ArtSev[ ma, yr ]
  				CongPrice.ClFc[ "Ext", "Art" ] <- CongPriceParm_Va..$ArtExt[ ma, yr ]
  
  				# Calculate congestion results
  				CongResults_ <- calcCongestion( 
  					CongModel_ = CongModel_, 
  					Dvmt.Ty = Dvmt.MaTy[ ma, ],
  					PerCapFwy = FwyLnMiCap.Ma[ ma ],                           
  					PerCapArt = ArtLnMiCap.Ma[ ma ], 
  					Pop = Pop.Ma[ ma ],                                 
  					BasePop = MetropolitanPop.MaYr[ ma, BaseYear ],
  					FwyArtProp = MpoBaseDvmtParm..Ma[ ma, "FwyArtProp" ], 
  					BusVmtSplit.Fc = TruckBusFcDvmtSplit_Va..$BusVmt[ ma, ], 
  					TruckVmtSplit.Fc = TruckBusFcDvmtSplit_Va..$TruckVmt[ ma, ], 
  					OpsDeployParm_Va.MaYr = OpsDeployParm_Va.MaYr, 
  					SmoothEcoDriveParm_Va.. = SmoothEcoDriveParm_Va..,
  					OtherOps_Yr.LvTy = OtherOps_Yr.LvTy, 
  					CongPrice.ClFc = CongPrice.ClFc, 
  					CongEfficiency.YrPt = CongEfficiency.YrPt, 
  					ValueOfTime = ValueOfTime,
  					ma=ma )
  
  				# Insert results in arrays
  				MpgMpkwhAdj.MaPt[ ma, ] <- CongResults_$MpgMpkwhAdj.Pt
  				VehHr.MaTy[ ma, ] <- CongResults_$VehHr.Ty
  				AveSpeed.MaTy[ ma, ] <- CongResults_$AveSpeed.Ty
  				FfVehHr.MaTy[ ma, ] <- CongResults_$FfVehHr.Ty
  				DelayVehHr.MaTy[ ma, ] <- CongResults_$DelayVehHr.Ty
  				CongVmt.ClFcTyMa[ , , "LtVeh" , ma ] <- CongResults_$LtVehDvmt.ClFc
  				CongVmt.ClFcTyMa[ , , "Truck" , ma ] <- CongResults_$TruckDvmt.ClFc
  				CongVmt.ClFcTyMa[ , , "Bus" , ma ] <- CongResults_$BusDvmt.ClFc
  				AveCongTaxPerMi.Ma[ ma ] <- CongResults_$AveCongTaxPerMi                    
  
                 # Clean up
                 rm( CongResults_ )
               
			}

  			# Save the results
  			#-----------------
  			Filename <- paste( RunYearDir, "/", "CommVehDvmt.CoDt", ".RData", sep="" )
  			save( CommVehDvmt.CoDt, file=Filename )
  			Filename <- paste( RunYearDir, "/", "CommVehDvmt.Ma", ".RData", sep="" )
  			save( CommVehDvmt.Ma, file=Filename )
  			Filename <- paste( RunYearDir, "/", "Dvmt.MaTy", ".RData", sep="" )
  			save( Dvmt.MaTy, file=Filename )
  			Filename <- paste( RunYearDir, "/", "MpgMpkwhAdj.MaPt", ".RData", sep="" )
  			save( MpgMpkwhAdj.MaPt, file=Filename )
  			Filename <- paste( RunYearDir, "/", "VehHr.MaTy", ".RData", sep="" )
  			save( VehHr.MaTy, file=Filename )
  			Filename <- paste( RunYearDir, "/", "AveSpeed.MaTy", ".RData", sep="" )
  			save( AveSpeed.MaTy, file=Filename )
  			Filename <- paste( RunYearDir, "/", "FfVehHr.MaTy", ".RData", sep="" )
  			save( FfVehHr.MaTy, file=Filename )
  			Filename <- paste( RunYearDir, "/", "DelayVehHr.MaTy", ".RData", sep="" )
  			save( DelayVehHr.MaTy, file=Filename )
  			Filename <- paste( RunYearDir, "/", "CongVmt.ClFcTyMa", ".RData", sep="" )
  			save( CongVmt.ClFcTyMa, file=Filename )
  			Filename <- paste( RunYearDir, "/", "AveCongTaxPerMi.Ma", ".RData", sep="" )
  			save( AveCongTaxPerMi.Ma, file=Filename )

        CommVehDvmt.CoDt <<- CommVehDvmt.CoDt
        CommVehDvmt.Ma <<- CommVehDvmt.Ma
        TruckDvmt <<- TruckDvmt
        TruckDvmt.Ma <<- TruckDvmt.Ma
        BusDvmt.Ma <<- BusDvmt.Ma
  			MpgMpkwhAdj.MaPt <<- MpgMpkwhAdj.MaPt
  			AveCongTaxPerMi.Ma <<- AveCongTaxPerMi.Ma
  			LtVehDvmt.Ma <<- LtVehDvmt.Ma
  			HhRoadDvmt.Ma <<- HhDvmt.Ma * LtVehDvmtFactor.Ma
  			Dvmt.MaTy <<- Dvmt.MaTy
		       
      } )


		#Step 4d: Calculate Commercial Service Vehicle Fuel Consumption, Emissions, Costs
		#================================================================================

			# Do calculations in local function to reduce clutter in workspace
			CommServ_ <- local( {

			# Split into components
			#----------------------
			# Split into auto and light truck components
			CommServLtTruckDvmt.CoDt <- CommVehDvmt.CoDt * CommServiceLtTruckProp.Yr[ yr ]
			CommServAutoDvmt.CoDt <- CommVehDvmt.CoDt - CommServLtTruckDvmt.CoDt
			# Calculate the commercial service auto age distribution
			CommServAutoAgProp.Ag <- adjustHvyVehAgeDistribution( VehProp_$AgCumProp.AgTy[ , "Auto" ], 
				AdjRatio=AgeAdj.YrTy[ yr, "CommAuto" ] )
			# Calculate commercial service light truck age distribution
			CommServLtTruckAgProp.Ag <- adjustHvyVehAgeDistribution( VehProp_$AgCumProp.AgTy[ , "LtTruck" ], 
				AdjRatio=AgeAdj.YrTy[ yr, "CommLtTruck" ] )
 			# Calculate index for model year information
			AgeIndexEnd <- which( rownames( CommServicePtProp..Yr ) == yr )
			AgeIndexStart <- AgeIndexEnd - length( CommServAutoAgProp.Ag ) + 1
			if( AgeIndexStart > 0 ) {
				AgeIndex. <- AgeIndexEnd:AgeIndexStart
			} else {
				AgeIndex. <- c( AgeIndexEnd:1, rep( 1, length( CommServAutoAgProp.Ag ) - AgeIndexEnd ) )
			}
			rm( AgeIndexEnd, AgeIndexStart )	
			# Calculate proportions of autos by powertrain
			AutoProp.AgPt <- CommServicePtProp..Yr[ AgeIndex., c( "AutoIceEco", "AutoIceNonEco", "AutoHev", "AutoEv" ) ] 
			CommServAutoProp.Pt <- colSums( sweep( AutoProp.AgPt, 1, CommServAutoAgProp.Ag, "*" ) )
			rm( AutoProp.AgPt )
			# Calculate proportions of light trucks by powertrain
			LtTruckProp.AgPt <- CommServicePtProp..Yr[ AgeIndex., c( "LtTruckIceEco", "LtTruckIceNonEco", "LtTruckHev", "LtTruckEv" ) ] 
			CommServLtTruckProp.Pt <- colSums( sweep( LtTruckProp.AgPt, 1, CommServLtTruckAgProp.Ag, "*" ) )
			rm( LtTruckProp.AgPt )

			# Calculate average MPG and MPKWH by county and development type
			#---------------------------------------------------------------
			# Calculate average MPG and MPKWH for autos by powertrain
			CommServAutoMpgMpkwh.YrPt <- cbind( AutoLtTrkMpg..Yr[ , "Auto" ], AutoLtTrkMpg..Yr[ , "Auto" ],
				HevMpgProp..Yr[ , "AutoHevMpg" ], EvRangeProp..Yr[ , "AutoMpkwh" ] )
			CommServAutoMpgMpkwh.AgPt <- CommServAutoMpgMpkwh.YrPt[ AgeIndex., ]
			CommServAutoMpgMpkwh.Pt <- colSums( sweep( CommServAutoMpgMpkwh.AgPt, 1, CommServAutoAgProp.Ag, "*" ) )
			names( CommServAutoMpgMpkwh.Pt ) <- names( CommServAutoProp.Pt )
			rm( CommServAutoMpgMpkwh.YrPt, CommServAutoMpgMpkwh.AgPt, CommServAutoAgProp.Ag )
			# Calculate average MPG and MPKWH for light trucks by powertrain
			CommServLtTruckMpgMpkwh.YrPt <- cbind( AutoLtTrkMpg..Yr[ , "LtTruck" ], AutoLtTrkMpg..Yr[ , "LtTruck" ],
				HevMpgProp..Yr[ , "LtTruckHevMpg" ], EvRangeProp..Yr[ , "LtTruckMpkwh" ] )
			CommServLtTruckMpgMpkwh.AgPt <- CommServLtTruckMpgMpkwh.YrPt[ AgeIndex., ]
			CommServLtTruckMpgMpkwh.Pt <- colSums( sweep( CommServLtTruckMpgMpkwh.AgPt, 1, CommServLtTruckAgProp.Ag, "*" ) )
			names( CommServLtTruckMpgMpkwh.Pt ) <- names( CommServLtTruckProp.Pt )
			rm( CommServLtTruckMpgMpkwh.YrPt, CommServLtTruckMpgMpkwh.AgPt, CommServLtTruckAgProp.Ag )			
			# Calculate average auto and light truck MPG and MPKWH by metropolitan area
			CommServAutoMpgMpkwh.MaPt <- sweep( MpgMpkwhAdj.MaPt[ , c( "LdIceEco", "LdIceNonEco", "LdHev", "LdEv" ) , drop=FALSE], 2, 
				CommServAutoMpgMpkwh.Pt, "*" )
			CommServLtTruckMpgMpkwh.MaPt <- sweep( MpgMpkwhAdj.MaPt[ , c( "LdIceEco", "LdIceNonEco", "LdHev", "LdEv" ) , drop=FALSE], 2, 
				CommServLtTruckMpgMpkwh.Pt, "*" )
			# Calculate average auto MPG by county and development type
			CommServAveAutoMpg <- sum( CommServAutoMpgMpkwh.Pt[1:3] * 
				( CommServAutoProp.Pt[1:3] ) / sum( CommServAutoProp.Pt[1:3] ) )
			CommServAveAutoMpg.Ma <- rowSums( sweep( CommServAutoMpgMpkwh.MaPt[,1:3,drop=FALSE], 2, 
				CommServAutoProp.Pt[1:3], "*" ) ) / sum( CommServAutoProp.Pt[1:3] )
			CommServAveAutoMpg.CoDt <- array( CommServAveAutoMpg, dim=c(length(Co),length(Dt)), dimnames=list(Co,Dt) )
			CommServAveAutoMpg.CoDt[ !is.na( CountyGroups..$Msa ), "Metropolitan" ] <- 
				CommServAveAutoMpg.Ma[ CountyGroups..$Msa[ !is.na( CountyGroups..$Msa ) ] ]
			rm( CommServAveAutoMpg, CommServAveAutoMpg.Ma )
			# Calculate average light truck MPG by county and development type
			CommServAveLtTruckMpg <- sum( CommServLtTruckMpgMpkwh.Pt[1:3] * 
				( CommServLtTruckProp.Pt[1:3] ) / sum( CommServLtTruckProp.Pt[1:3] ) )
			CommServAveLtTruckMpg.Ma <- rowSums( sweep( CommServLtTruckMpgMpkwh.MaPt[,1:3,drop=FALSE], 2, 
				CommServLtTruckProp.Pt[1:3], "*" ) ) / sum( CommServLtTruckProp.Pt[1:3] )
			CommServAveLtTruckMpg.CoDt <- array( CommServAveLtTruckMpg, dim=c(length(Co),length(Dt)), dimnames=list(Co,Dt) )
			CommServAveLtTruckMpg.CoDt[ !is.na( CountyGroups..$Msa ), "Metropolitan" ] <- 
				CommServAveLtTruckMpg.Ma[ CountyGroups..$Msa[ !is.na( CountyGroups..$Msa ) ] ]
			rm( CommServAveLtTruckMpg, CommServAveLtTruckMpg.Ma )
			# Calculate average auto MPKWH by county and development type
			CommServAveAutoMpkwh.CoDt	<- array( CommServAutoMpgMpkwh.Pt[4], dim=c(length(Co),length(Dt)), dimnames=list(Co,Dt) )
			CommServAveAutoMpkwh.CoDt[ !is.na( CountyGroups..$Msa ), "Metropolitan" ] <- 
				CommServAutoMpgMpkwh.MaPt[ CountyGroups..$Msa[ !is.na( CountyGroups..$Msa ) ], 4 ]
			# Calculate average light truck MPKWH by county and development type
			CommServAveLtTruckMpkwh.CoDt	<- array( CommServLtTruckMpgMpkwh.Pt[4], 
				dim=c(length(Co),length(Dt)), dimnames=list(Co,Dt) )
			CommServAveLtTruckMpkwh.CoDt[ !is.na( CountyGroups..$Msa ), "Metropolitan" ] <- 
				CommServLtTruckMpgMpkwh.MaPt[ CountyGroups..$Msa[ !is.na( CountyGroups..$Msa ) ], 4 ]

			# Calculate proportions of DVMT using fuel vs. electricity
			#---------------------------------------------------------
			CommServAutoHcDvmt.CoDt <- CommServAutoDvmt.CoDt * sum( CommServAutoProp.Pt[1:3] ) 				 										                                                                                                         
			CommServAutoEvDvmt.CoDt <- CommServAutoDvmt.CoDt * sum( CommServAutoProp.Pt[4] ) 				 										                                                                                                         
			CommServLtTruckHcDvmt.CoDt <- CommServLtTruckDvmt.CoDt * sum( CommServLtTruckProp.Pt[1:3] )
			CommServLtTruckEvDvmt.CoDt <- CommServLtTruckDvmt.CoDt * sum( CommServLtTruckProp.Pt[4] )
			
			# Calculate proportions of DVMT using ICE vs Alt Powertrains (for Dashboard)
			#---------------------------------------------------------------------------
			CommServAutoIceDvmt.CoDt <- CommServAutoDvmt.CoDt * sum( CommServAutoProp.Pt[1] ) 				 										                                                                                                         
			CommServAutoNonIceDvmt.CoDt <- CommServAutoDvmt.CoDt * sum( CommServAutoProp.Pt[2:4] ) 				 										                                                                                                         
			CommServLtTruckIceDvmt.CoDt <- CommServLtTruckDvmt.CoDt * sum( CommServLtTruckProp.Pt[1] )
			CommServLtTruckNonIceDvmt.CoDt <- CommServLtTruckDvmt.CoDt * sum( CommServLtTruckProp.Pt[2:4] )
			
			# Calculate commercial service vehicle fuel consumption by type
			#--------------------------------------------------------------
			# Calculate auto fuel consumption by type
			CommServAutoFuel.CoDt <- CommServAutoHcDvmt.CoDt / CommServAveAutoMpg.CoDt
			AutoFuelProp.Ft <- numeric(5)
			names( AutoFuelProp.Ft ) <- Ft
			AutoFuelProp.Ft[ "ULSD" ] <- AutoLtTrkFuels..Yr[ yr, "AutoPropDiesel" ] * 
				( 1 - AutoLtTrkFuels..Yr[ yr, "DieselPropBio" ] )
			AutoFuelProp.Ft[ "Biodiesel" ] <- AutoLtTrkFuels..Yr[ yr, "AutoPropDiesel" ] * 
				( AutoLtTrkFuels..Yr[ yr, "DieselPropBio" ] )
			AutoFuelProp.Ft[ "CNG" ] <- AutoLtTrkFuels..Yr[ yr, "AutoPropCng" ]
			AutoFuelProp.Ft[ "Gasoline" ] <- ( 1 - sum( AutoFuelProp.Ft[ c( "ULSD", "Biodiesel", "CNG" ) ] ) ) *
				( 1 - AutoLtTrkFuels..Yr[ yr, "GasPropEth" ] )
			AutoFuelProp.Ft[ "Ethanol" ] <- ( 1 - sum( AutoFuelProp.Ft[ c( "ULSD", "Biodiesel", "CNG" ) ] ) ) *
				( AutoLtTrkFuels..Yr[ yr, "GasPropEth" ] )
			CommServAutoFuel.CoDtFt <- outer( CommServAutoFuel.CoDt, AutoFuelProp.Ft, "*" )
			rm( AutoFuelProp.Ft )
			# Calculate light truck fuel consumption by type
			CommServLtTruckFuel.CoDt <- CommServLtTruckHcDvmt.CoDt / CommServAveLtTruckMpg.CoDt
			LtTruckFuelProp.Ft <- numeric(5)
			names( LtTruckFuelProp.Ft ) <- Ft
			LtTruckFuelProp.Ft[ "ULSD" ] <- AutoLtTrkFuels..Yr[ yr, "LtTrkPropDiesel" ] * 
				( 1 - AutoLtTrkFuels..Yr[ yr, "DieselPropBio" ] )
			LtTruckFuelProp.Ft[ "Biodiesel" ] <- AutoLtTrkFuels..Yr[ yr, "LtTrkPropDiesel" ] * 
				( AutoLtTrkFuels..Yr[ yr, "DieselPropBio" ] )
			LtTruckFuelProp.Ft[ "CNG" ] <- AutoLtTrkFuels..Yr[ yr, "LtTrkPropCng" ]
			LtTruckFuelProp.Ft[ "Gasoline" ] <- ( 1 - sum( LtTruckFuelProp.Ft[ c( "ULSD", "Biodiesel", "CNG" ) ] ) ) *
				( 1 - AutoLtTrkFuels..Yr[ yr, "GasPropEth" ] )
			LtTruckFuelProp.Ft[ "Ethanol" ] <- ( 1 - sum( LtTruckFuelProp.Ft[ c( "ULSD", "Biodiesel", "CNG" ) ] ) ) *
				( AutoLtTrkFuels..Yr[ yr, "GasPropEth" ] )
			CommServLtTruckFuel.CoDtFt <- outer( CommServLtTruckFuel.CoDt, LtTruckFuelProp.Ft, "*" )
			rm( LtTruckFuelProp.Ft )
			
			# Calculate emissions per gallon of fuel consumed
			#------------------------------------------------
	     	FuelCo2.Ft <- numeric(5)
			names( FuelCo2.Ft ) <- Ft
			FuelCo2Input. <- unlist( Inputs_$FuelCo2..Yr[ yr, ] )
			FuelCo2.Ft[ "ULSD" ] <- FuelCo2Input.[ "ULSD" ]
			FuelCo2.Ft[ "Biodiesel" ] <- FuelCo2Input.[ "Biodiesel" ]
			if( yr == "1990" ) {
		     	FuelCo2.Ft[ "Gasoline" ] <- FuelCo2Input.[ "RFG" ]
			} else {
		     	FuelCo2.Ft[ "Gasoline" ] <- FuelCo2Input.[ "CARBOB" ]
			}
			FuelCo2.Ft[ "Ethanol" ] <- FuelCo2Input.[ "Ethanol" ]
			FuelCo2.Ft[ "CNG" ] <- FuelCo2Input.[ "Cng" ]
	
			# Calculate auto and light truck emissions from fuel consumption
			#---------------------------------------------------------------
			# Calculate auto emissions
			AutoHcMj.CoDtFt <- CommServAutoFuel.CoDtFt * MjPerGallon
			CommServAutoHcCo2e.CoDtFt <- sweep( AutoHcMj.CoDtFt, 3, FuelCo2.Ft, "*" ) / 1000000
			CommServAutoHcCo2e.CoDt <- apply( CommServAutoHcCo2e.CoDtFt, c(1,2), sum )
			rm( AutoHcMj.CoDtFt, CommServAutoHcCo2e.CoDtFt )
			# Calculate light truck emissions
			LtTruckHcMj.CoDtFt <- CommServLtTruckFuel.CoDtFt * MjPerGallon
			CommServLtTruckHcCo2e.CoDtFt <- sweep( LtTruckHcMj.CoDtFt, 3, FuelCo2.Ft, "*" ) / 1000000
			CommServLtTruckHcCo2e.CoDt <- apply( CommServLtTruckHcCo2e.CoDtFt, c(1,2), sum )
			rm( LtTruckHcMj.CoDtFt, CommServLtTruckHcCo2e.CoDtFt )

			# Calculate auto and light truck emissions from electricity consumption
			#----------------------------------------------------------------------
			# Calculate power consumed
			CommServAutoKwh.CoDt <- CommServAutoEvDvmt.CoDt / CommServAveAutoMpkwh.CoDt
			CommServAutoKwh.CoDt[ is.nan( CommServAutoKwh.CoDt ) ] <- 0
			CommServLtTruckKwh.CoDt <- CommServLtTruckEvDvmt.CoDt / CommServAveLtTruckMpkwh.CoDt
			CommServLtTruckKwh.CoDt[ is.nan( CommServLtTruckKwh.CoDt ) ] <- 0
			# Calculate total emissions by metropolitan area
			CommServAutoEvCo2e.CoDt <- sweep( CommServAutoKwh.CoDt, 1, PowerCo2.CoYr[,yr], "*" ) / 2204.62262
			CommServLtTruckEvCo2e.CoDt <- sweep( CommServLtTruckKwh.CoDt, 1, PowerCo2.CoYr[,yr], "*" ) / 2204.62262

			# Total amounts to be used in cost calculations and saved
			#--------------------------------------------------------
			CommServDvmt <- sum( CommVehDvmt.CoDt )
			CommServDvmt.Ma <- tapply( CommServAutoDvmt.CoDt[,"Metropolitan"] + CommServLtTruckDvmt.CoDt[,"Metropolitan"],
				CountyGroups..$Msa, sum )[ Ma ]
			CommServFuel <- sum( CommServLtTruckFuel.CoDtFt + CommServLtTruckFuel.CoDtFt )
			CommServFuel.Dt <- apply( CommServLtTruckFuel.CoDtFt + CommServLtTruckFuel.CoDtFt, 3, sum )   
			CommServKwh <- sum( CommServAutoKwh.CoDt + CommServLtTruckKwh.CoDt )
			CommServCo2e <- sum( CommServAutoHcCo2e.CoDt + CommServAutoEvCo2e.CoDt + CommServLtTruckHcCo2e.CoDt +
				CommServLtTruckEvCo2e.CoDt )

			# Calculate commercial service vehicle costs
			#-------------------------------------------
			if( it == 1 ) {
				VmtSurcharge <- 0
			} else {
				VmtSurcharge <- VmtSurcharge.It[ it - 1 ]   # VmtSurcharge is value calculated in previous iteration
			}
			Costs. <- Costs.YrCs[yr,]
			# Calculate VMT-based costs                                  
			VmtExtCost <- CommServDvmt * sum( Costs.[ c( "AirPollution", "OtherResource", "Safety", "Noise" ) ] )
			FuelExtCost <- CommServFuel * Costs.[ "EnergySecurity" ]
			Co2eExtCost <- CommServCo2e * Costs.[ "ClimateChange" ]
			TotExtCost <- VmtExtCost + FuelExtCost + Co2eExtCost
			# Calculate carbon tax
			CarbonTax <- CommServCo2e * Costs.[ "CarbonTax" ]
			# Calculate externality taxes exclusive of any carbon tax that is paid
			AddedExtTax <- TotExtCost * Costs.[ "PropExtPaid" ] - CarbonTax		
			# Calculate daily fuel and power costs
			FuelCost <- CommServFuel * Costs.[ "FuelCost" ]
			PowerCost <- CommServKwh * Costs.[ "KwhCost" ]
			# Calculate vehicle road system use taxes
			GasTax <- CommServFuel * Costs.[ "GasTax" ]
			CongTax <- sum( CommServDvmt.Ma * AveCongTaxPerMi.Ma )
			VmtTax <- CommServDvmt * ( Costs.[ "VmtTax" ] + VmtSurcharge * Costs.YrCs[yr,"VmtSurchargeMultiplier"] )
			RoadUseTax <- GasTax + CongTax + VmtTax
			# Calculate total road expenses (to build, repair, operate, etc.)
			BaseModCost <- CommServDvmt * Costs.[ "BaseMod" ]
			PresOpMaintCost <- CommServDvmt * Costs.[ "PresOpMaint" ]
			OtherRoadCost <- CommServDvmt * Costs.[ "OtherRoad" ]
			ExtModCost <- CommServDvmt * ExtraModCost
			TotRoadCost <- BaseModCost + PresOpMaintCost + OtherRoadCost + ExtModCost
			# Put into vector
			CommServCosts. <- c( TotExtCost, CarbonTax, AddedExtTax, FuelCost, PowerCost, RoadUseTax, TotRoadCost ) 
			names( CommServCosts. ) <-  c( "TotExtCost", "CarbonTax", "AddedExtTax", "FuelCost", "PowerCost", 
				"RoadUseTax", "TotRoadCost" )

			# Return the commercial service vehicle results from local function
			#------------------------------------------------------------------			
			list( CommServDvmt=CommServDvmt, CommServDvmt.Ma=CommServDvmt.Ma, CommServFuel=CommServFuel,
				CommServFuel.Dt=CommServFuel.Dt, CommServKwh=CommServKwh, CommServCo2e=CommServCo2e,
				CommServCosts.=CommServCosts., CommServAutoIceDvmt.CoDt = CommServAutoIceDvmt.CoDt,  
				CommServAutoNonIceDvmt.CoDt = CommServAutoNonIceDvmt.CoDt, CommServLtTruckIceDvmt.CoDt = CommServLtTruckIceDvmt.CoDt,
				CommServLtTruckNonIceDvmt.CoDt = CommServLtTruckNonIceDvmt.CoDt
			)				

			} )						

			# Save the results
			#-----------------
			Filename <- paste( RunYearDir, "/", "CommServ_", ".RData", sep="" )
			save( CommServ_, file=Filename )


		#Step 4e: Calculate Total Costs and the VMT Surcharge to Pay for Infrastructure
		#==============================================================================

			# Calculate total costs
			#----------------------
			# Calculate total household road cost, adjusting for DVMT adjustment
			DvmtAdjRatio <- sum( CostSummary.CoVa[ , "AdjDvmt" ] ) / sum( CostSummary.CoVa[ , "Dvmt" ] )  
			TotHhRoadCost <- sum( CostSummary.CoVa[ , "TotRoadCost" ] ) * DvmtAdjRatio
			# Calculate light vehicle DVMT
			LtVehDvmt <- sum( CostSummary.CoVa[ , "AdjDvmt" ] ) + CommServ_$CommServDvmt
			# First iteration, calculate the extra modernization cost for new lanes (ExtraModCost)
			if( it == 1 ) {
				HvyVehDvmtEq <- sum(TruckDvmt) * CongModel_$Pce.Ty["Truck"] + sum( BusDvmt.Ma ) * CongModel_$Pce.Ty["Bus"]
				LtVehAddCostProp <- LtVehDvmt / ( LtVehDvmt + HvyVehDvmtEq ) 
				LnMiAddCost <- LtVehAddCostProp * AnnLnMiAddCosts.Yr[yr] / 365
				ExtraModCost <- LnMiAddCost / LtVehDvmt 
				TotRoadCost <- TotHhRoadCost + CommServ_$CommServCosts.[ "TotRoadCost" ] + LnMiAddCost
				rm( HvyVehDvmtEq, LtVehAddCostProp, LnMiAddCost )
			# Otherwise sum household & commercial vehicle costs because they include the added lane-mile costs 
			} else {
				TotRoadCost <- TotHhRoadCost + CommServ_$CommServCosts.[ "TotRoadCost" ]
			}
			
			# Calculate total revenues
			#-------------------------
			# Calculate total household revenues, adjusting for DVMT adjustment
			TotHhRoadUseTax <- sum( CostSummary.CoVa[ , "RoadUseTax" ] ) * DvmtAdjRatio
			# Add in estimated congestion tax if 1st iteration (since costs calculated before congestion tax)
			if( it == 1 ) {
				TotHhRoadUseTax <- TotHhRoadUseTax + sum( HhRoadDvmt.Ma * AveCongTaxPerMi.Ma )
			}			
			# Add in commercial light service vehicle
			TotRoadUseTax <- TotHhRoadUseTax + CommServ_$CommServCosts.["RoadUseTax"]

			# Compare total costs to revenues and calculate VMT surcharge to pay for system
			#------------------------------------------------------------------------------
			# This procedure calculates how much to increase a VMT tax to pay for system costs
			# It includes guards to keep the procedure from adding a negative surcharge that counteracts
			# VMT tax assumptions that are inputs to the model.
			# Calculate the gap between taxes and costs
			TaxGap <- TotRoadCost - TotRoadUseTax
			# If this is the first iteration, the tax gap per mile is added to the starting surcharge of 0

      if( it == 1 ) {
				# Calculate a VmtSurcharge only if there is a positive tax gap
				if( TaxGap > 0 ) {
					VmtSurcharge.It[ it ] <- TaxGap / LtVehDvmt
				# If the tax gap is negative, the VMT surcharge is zero
				} else {
					VmtSurcharge.It[ it ] <- 0
				}
			# If later iteration, add the calculated tax gap per mile to the previous surcharge
			} else {
				# If the VmtSurcharge for the previous iteration is positive, 
				# calculate the added surcharge for the iteration
				if( VmtSurcharge.It[ it - 1 ] > 0 ) { 
					VmtSurcharge.It[ it ] <- VmtSurcharge.It[ it - 1 ] +  TaxGap / LtVehDvmt
				# Otherwise the VmtSurcharge is zero
				} else {
					VmtSurcharge.It[ it ] <- 0
				}
			}
			
			# Clean up
			#---------
			RoadCostSummary. <- c( TotRoadCost=TotRoadCost, TotRoadUseTax=TotRoadUseTax, TaxGap=TaxGap )
			Filename <- paste( RunYearDir, "/", "RoadCostSummary.", ".RData", sep="" )
			save( RoadCostSummary., file=Filename )
			rm( DvmtAdjRatio, TotHhRoadCost, LtVehDvmt, TotRoadCost, TotHhRoadUseTax, TotRoadUseTax )			
			gc()
			print( memory.size() )
		
	
		#Step 4f: Adjust MPG & MPKWH
		#===========================
		
			# Only do this once, on the first iteration
			# Otherwise ecodriving would keep increasing MPG with each iteration
			if( it == 1 ) {
			          
				StartTime <- Sys.time()
				print( paste( "Iteration", it, "Adjust MPG & MPKWH for ecodriving etc." ) )

   			for( co in Co ) {
     	
   				print( co )

					# Load county file
					Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
					SynPop.. <- assignLoad( Filename )
					rm( Filename )
					# Identify metropolitan area
					MetroArea <- CountyGroups..[ co, "Msa" ]
					IsMetro. <- SynPop..$DevType == "Metropolitan"

					# Calculate adjustments
					HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
					ModelVar. <- c( "Houseid", "Hhvehcnt", "IsEcoDriver", "IsLowRollTire", "Powertrain", 
						"VehMpg", "VehMpkwh" )

					# Get adjustments by powertrain for area
					# Note that non-mpo areas are average of smaller MPO areas (generalized to the smallest 3 MPO areas based on population)
          SmallMpos <- row.names(MetropolitanPop.MaYr[order(MetropolitanPop.MaYr[,yr]),,drop=FALSE])[1:min(length(Ma),3)]   
             
					if( is.na( MetroArea ) ) {
						MpgMpkwhAdj.Pt <- colMeans( MpgMpkwhAdj.MaPt[ SmallMpos, ,drop=FALSE] )
					} else {
						MpgMpkwhAdj.Pt <- MpgMpkwhAdj.MaPt[ MetroArea, ]
					}
					MpgMpkwhAdj_ <- adjEcoTire( Data..=SynPop..[ HasVeh.Hh, ModelVar. ], 
						MpgMpkwhAdj.Pt=MpgMpkwhAdj.Pt, 
						TireMpgImp=EcoTire..Yr[yr,"TireMpgImp"], 
						TireMpkwhImp=EcoTire..Yr[yr,"TireMpkwhImp"] )
					rm( ModelVar. )
					# Assign to households
					SynPop..$VehMpg[ HasVeh.Hh ] <- MpgMpkwhAdj_$VehMpg_
					SynPop..$VehMpkwh[ HasVeh.Hh ] <- MpgMpkwhAdj_$VehMpkwh_
					rm( HasVeh.Hh, MpgMpkwhAdj_ )

					# Save the household dataset
 					Filename <- paste( RunYearDir, "/", co, ".RData", sep="" )
					save( SynPop.., file=Filename, compress=TRUE )
					rm( SynPop.., IsMetro., MetroArea )
					gc()
					gc()
					print( memory.size() )

				}

				print( StartTime )
				print( Sys.time() )

			}

  		Filename <- paste( RunYearDir, "/", "VmtSurcharge.It", ".RData", sep="" )
  		save( VmtSurcharge.It, file=Filename )
  		rm( Filename )
	
	# End of for loop to equilibrate DVMT, congestion, costs and road use taxes
	}

#==================================================================================
#STEP 5: CALCULATE METROPOLITAN AREA MEDIUM/HEAVY VEHICLE CONSUMPTION AND EMISSIONS
#==================================================================================

	# Calculate truck and bus age distributions
	#------------------------------------------
	# Calculate the truck age distribution
	MedTruckAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "MedTruck" ], 
		                                                AdjRatio=AgeAdj.YrTy[ yr, "MedTruck" ] )
	HvyTruckAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "HvyTruck" ], 
	                                                  AdjRatio=AgeAdj.YrTy[ yr, "HvyTruck" ] )
	# Calculate bus age distribution
	BusAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "Bus" ], 
		AdjRatio=AgeAdj.YrTy[ yr, "Bus" ] )
        
	# Calculate truck and bus fuel economy
	#-------------------------------------
	# Calculate truck fuel economy
	MedTruckMpg <- assignHvyVehFuelEconomy( MedTruckAgProp.Ag, Mpg..Yr=HvyVehMpgMpk..Yr,
		                                      Type="MedTruck", CurrYear=yr )
	HvyTruckMpg <- assignHvyVehFuelEconomy( HvyTruckAgProp.Ag, Mpg..Yr=HvyVehMpgMpk..Yr,
	                                        Type="HvyTruck", CurrYear=yr )
	TruckMpg <- c(Medium = MedTruckMpg, Heavy = HvyTruckMpg)
	rm( MedTruckMpg, HvyTruckMpg )
	
	# Calculate bus fuel economy
	BusMpg <- assignHvyVehFuelEconomy( BusAgProp.Ag, Mpg..Yr=HvyVehMpgMpk..Yr, Type="Bus", 
		CurrYear=yr )
	# Adjust fuel economy to account for congestion, etc for metropolitan areas
	MedTruckMpg.Ma <- MpgMpkwhAdj.MaPt[ , "TruckIce" ] * TruckMpg["Medium"]
	HvyTruckMpg.Ma <- MpgMpkwhAdj.MaPt[ , "TruckIce" ] * TruckMpg["Heavy"]
	TruckMpg.Ma <- cbind(Medium = MedTruckMpg.Ma, Heavy = HvyTruckMpg.Ma)
	rm( MedTruckMpg.Ma, HvyTruckMpg.Ma)
	BusMpg.Ma <- MpgMpkwhAdj.MaPt[ , "BusIce" ] * BusMpg
	# Calculate fuel economy for non-metropolitan areas
   	SmallMpos <- row.names(MetropolitanPop.MaYr[order(MetropolitanPop.MaYr[,yr]),,drop=FALSE])[1:min(length(Ma),3)]   
	NonMpoTruckMpg <- mean( MpgMpkwhAdj.MaPt[ SmallMpos, "TruckIce" ] )  * TruckMpg
	
	# Calculate truck fuel consumption by fuel type
	#----------------------------------------------
	# Calculate overall fuel consumption
	names(TruckProp) <- c("Medium", "Heavy")
	MedTruckFuel.Ma <- Dvmt.MaTy[,"Truck"] * TruckProp["Medium"] / TruckMpg.Ma[, "Medium"]
	HvyTruckFuel.Ma <- Dvmt.MaTy[,"Truck"] * TruckProp["Heavy"] / TruckMpg.Ma[, "Heavy"]
	NonMpoTruckDvmt <- TruckDvmt - sum( Dvmt.MaTy[,"Truck"] ) * TruckProp
	NonMpoTruckFuel <- NonMpoTruckDvmt / NonMpoTruckMpg
	# Calculate fuel consumption by type
	TruckFuelProp.Ft <- matrix(nrow = length(Ft), ncol = length(TruckProp))
	rownames( TruckFuelProp.Ft ) <- Ft
	colnames( TruckFuelProp.Ft ) <- names(TruckProp)
	TruckFuelInput. <- cbind( Medium = unlist( MedTruckFuels..Yr[yr,] ),
	                          Heavy = unlist( HvyTruckFuels..Yr[yr,] ) )
	PropDiesel <- 1 - TruckFuelInput.[ "PropGas", ] - TruckFuelInput.[ "PropCng", ]
	TruckFuelProp.Ft[ "ULSD", ] <- PropDiesel * ( 1 - TruckFuelInput.[ "DieselPropBio", ] )
	TruckFuelProp.Ft[ "Biodiesel", ] <- PropDiesel * ( TruckFuelInput.[ "DieselPropBio", ] )
	TruckFuelProp.Ft[ "Gasoline", ] <- ( TruckFuelInput.[ "PropGas", ] ) *
		( 1 - TruckFuelInput.[ "GasPropEth", ] )
	TruckFuelProp.Ft[ "Ethanol", ] <- ( TruckFuelInput.[ "PropGas", ] ) *
		( TruckFuelInput.[ "GasPropEth", ] )
	TruckFuelProp.Ft[ "CNG", ] <- ( TruckFuelInput.[ "PropCng", ] )
	MedTruckFuel.MaFt <- outer( MedTruckFuel.Ma, TruckFuelProp.Ft[, "Medium"], "*" )
	HvyTruckFuel.MaFt <- outer( HvyTruckFuel.Ma, TruckFuelProp.Ft[, "Heavy"], "*" )
	TruckFuel.MaFt <- abind( Medium = MedTruckFuel.MaFt, Heavy = HvyTruckFuel.MaFt, along = 3 )
  # if (length(Ma)==1) dimnames(TruckFuel.MaFt)[[1]]<-Ma #to replace dropped name
	NonMpoTruckFuel.Ft <- t(NonMpoTruckFuel * t(TruckFuelProp.Ft))
	rm( TruckFuelInput., PropDiesel, TruckFuelProp.Ft, MedTruckFuel.MaFt, HvyTruckFuel.MaFt )

	# Calculate Bus Fuel Consumption and Emissions
	#---------------------------------------------
	# Calculate overall fuel consumption
	BusFuel.Ma <- Dvmt.MaTy[,"Bus"] / BusMpg.Ma
	rm( BusMpg.Ma )
	# Calculate fuel consumption by type
	BusFuelProp.MaFt <- array( 0, dim=c( length(Ma), length(Ft) ), dimnames=list( Ma, Ft ) )
	for( ma in Ma ) {
		BusFuelProp.Ft <- numeric(5)
		names( BusFuelProp.Ft ) <- Ft
		BusFuelInput. <- unlist( BusFuels.FpYrMa[,yr,ma] )
		PropDiesel <- 1 - BusFuelInput.[ "PropGas" ] - BusFuelInput.[ "PropCng" ]
		BusFuelProp.Ft[ "ULSD" ] <- PropDiesel * ( 1 - BusFuelInput.[ "DieselPropBio" ] )
		BusFuelProp.Ft[ "Biodiesel" ] <- PropDiesel * ( BusFuelInput.[ "DieselPropBio" ] )
		BusFuelProp.Ft[ "Gasoline" ] <- ( BusFuelInput.[ "PropGas" ] ) *
			( 1 - BusFuelInput.[ "GasPropEth" ] )
		BusFuelProp.Ft[ "Ethanol" ] <- ( BusFuelInput.[ "PropGas" ] ) *
			( BusFuelInput.[ "GasPropEth" ] )
		BusFuelProp.Ft[ "CNG" ] <- ( BusFuelInput.[ "PropCng" ] )
		BusFuelProp.MaFt[ ma, ] <- BusFuelProp.Ft
		rm( BusFuelInput., PropDiesel, BusFuelProp.Ft )
	}
	BusFuel.MaFt <- sweep( BusFuelProp.MaFt, 1, BusFuel.Ma, "*" )

	# Calculate emissions per gallon of fuel consumed
	#------------------------------------------------
	FuelCo2.Ft <- numeric(length(Ft))
	names( FuelCo2.Ft ) <- Ft
	FuelCo2Input. <- unlist( Inputs_$FuelCo2..Yr[ yr, ] )
	FuelCo2.Ft[ "ULSD" ] <- FuelCo2Input.[ "ULSD" ]
	FuelCo2.Ft[ "Biodiesel" ] <- FuelCo2Input.[ "Biodiesel" ]
	if( yr == "1990" ) {
		FuelCo2.Ft[ "Gasoline" ] <- FuelCo2Input.[ "RFG" ]
	} else {
		FuelCo2.Ft[ "Gasoline" ] <- FuelCo2Input.[ "CARBOB" ]
	}
	FuelCo2.Ft[ "Ethanol" ] <- FuelCo2Input.[ "Ethanol" ]
	FuelCo2.Ft[ "CNG" ] <- FuelCo2Input.[ "Cng" ]
	
	# Calculate truck fuel consumption reduction from AV/CV, platooning, HEVs, and eco-driving
	#------------------------------------------------
	
	# AV/CV/platooning
	TruckFuel.MaFt[, , "Medium"]   <- TruckFuel.MaFt[, , "Medium"]   * (1 - prod(TrkEcoAVCV..Yr[yr, c("AvCvPropVmtMed", "AvCvImpMed")]))
	TruckFuel.MaFt[, , "Heavy"]    <- TruckFuel.MaFt[, , "Heavy"]    * (1 - prod(TrkEcoAVCV..Yr[yr, c("AvCvPropVmtHvy", "AvCvImpHvy")]))
	NonMpoTruckFuel.Ft[, "Medium"] <- NonMpoTruckFuel.Ft[, "Medium"] * (1 - prod(TrkEcoAVCV..Yr[yr, c("AvCvPropVmtMed", "AvCvImpMed")]))
	NonMpoTruckFuel.Ft[, "Heavy"]  <- NonMpoTruckFuel.Ft[, "Heavy"]  * (1 - prod(TrkEcoAVCV..Yr[yr, c("AvCvPropVmtHvy", "AvCvImpHvy")]))
	
	# Eco-driving
	TruckFuel.MaFt[, , "Medium"]   <- TruckFuel.MaFt[, , "Medium"]   * (1 - prod(TrkEcoAVCV..Yr[yr, c("EcoDrvPropVmtMed", "EcoMpgImpMed")]))
	TruckFuel.MaFt[, , "Heavy"]    <- TruckFuel.MaFt[, , "Heavy"]    * (1 - prod(TrkEcoAVCV..Yr[yr, c("EcoDrvPropVmtHvy", "EcoMpgImpHvy")]))
	NonMpoTruckFuel.Ft[, "Medium"] <- NonMpoTruckFuel.Ft[, "Medium"] * (1 - prod(TrkEcoAVCV..Yr[yr, c("EcoDrvPropVmtMed", "EcoMpgImpMed")]))
	NonMpoTruckFuel.Ft[, "Heavy"]  <- NonMpoTruckFuel.Ft[, "Heavy"]  * (1 - prod(TrkEcoAVCV..Yr[yr, c("EcoDrvPropVmtHvy", "EcoMpgImpHvy")]))
	
	# HEV
	TruckFuel.MaFt[, , "Medium"]   <- TruckFuel.MaFt[, , "Medium"]   * (1 - prod(TrkHevMpgProp..Yr[yr, c("MedTruckPropHev", "MedTruckHevMpg")]))
	TruckFuel.MaFt[, , "Heavy"]    <- TruckFuel.MaFt[, , "Heavy"]    * (1 - prod(TrkHevMpgProp..Yr[yr, c("HvyTruckPropHev", "HvyTruckHevMpg")]))
	NonMpoTruckFuel.Ft[, "Medium"] <- NonMpoTruckFuel.Ft[, "Medium"] * (1 - prod(TrkHevMpgProp..Yr[yr, c("MedTruckPropHev", "MedTruckHevMpg")]))
	NonMpoTruckFuel.Ft[, "Heavy"]  <- NonMpoTruckFuel.Ft[, "Heavy"]  * (1 - prod(TrkHevMpgProp..Yr[yr, c("HvyTruckPropHev", "HvyTruckHevMpg")]))

	# Calculate truck and bus emissions
	#----------------------------------
	# Calculate truck emissions
	TruckMj.MaTy <- TruckFuel.MaFt * MjPerGallon
	TruckCo2e.MaTy <- sweep( TruckMj.MaTy, 2:3, FuelCo2.Ft, "*" ) / 1000000
	TruckCo2e.Ma <- apply( TruckCo2e.MaTy, MARGIN = c(1,3), FUN = sum)
	NonMpoTruckMj.Ty <- NonMpoTruckFuel.Ft * MjPerGallon
	NonMpoTruckCo2e <- colSums( NonMpoTruckMj.Ty * FuelCo2.Ft ) / 1000000
	rm( TruckMj.MaTy, TruckCo2e.MaTy, NonMpoTruckMj.Ty )
	# Calculate bus emissions
	BusMj.MaTy <- BusFuel.MaFt * MjPerGallon
	BusCo2e.MaTy <- sweep( BusMj.MaTy, 2, FuelCo2.Ft, "*" ) / 1000000
	BusCo2e.Ma <- rowSums( BusCo2e.MaTy )
	rm( BusMj.MaTy, BusCo2e.MaTy, FuelCo2.Ft )

	# Calculate rail emissions
	#-------------------------
	# Calculate DVMT and power consumed
	RailDvmt.Ma <- RailRevMi.Ma * TranAdjFactor / 365
	RailPower.Ma <- RailDvmt.Ma / HvyVehMpgMpk..Yr[yr,"Train"]
	# Calculate average emissions per kwh by metropolitan area
	CoToMa. <- CountyGroups..$Msa
	names( CoToMa. ) <- rownames( CountyGroups.. )
	CoToMa. <- CoToMa.[ !is.na( CoToMa. ) ]
	PowerCo2.Ma <- tapply( PowerCo2.CoYr[ names( CoToMa. ), yr ], CoToMa., mean )[ Ma ]
	rm( CoToMa. )
	# Calculate total emissions by metropolitan area
	RailCo2e.Ma <- RailPower.Ma * PowerCo2.Ma / 2204.62262
	rm( RailDvmt.Ma, PowerCo2.Ma )

	# Save the results
	#-----------------
	Filename <- paste( RunYearDir, "/", "TruckFuel.MaFt", ".RData", sep="" )
	save( TruckFuel.MaFt, file=Filename )
	Filename <- paste( RunYearDir, "/", "NonMpoTruckDvmt", ".RData", sep="" )
	save( NonMpoTruckDvmt, file=Filename )
	Filename <- paste( RunYearDir, "/", "NonMpoTruckFuel.Ft", ".RData", sep="" )
	save( NonMpoTruckFuel.Ft, file=Filename )
	Filename <- paste( RunYearDir, "/", "BusFuel.MaFt", ".RData", sep="" )
	save( BusFuel.MaFt, file=Filename )
	Filename <- paste( RunYearDir, "/", "TruckCo2e.Ma", ".RData", sep="" )
	save( TruckCo2e.Ma, file=Filename )
	Filename <- paste( RunYearDir, "/", "NonMpoTruckCo2e", ".RData", sep="" )
	save( NonMpoTruckCo2e, file=Filename )
	Filename <- paste( RunYearDir, "/", "BusCo2e.Ma", ".RData", sep="" )
	save( BusCo2e.Ma, file=Filename )
	Filename <- paste( RunYearDir, "/", "RailPower.Ma", ".RData", sep="" )
	save( RailPower.Ma, file=Filename )
	Filename <- paste( RunYearDir, "/", "RailCo2e.Ma", ".RData", sep="" )
	save( RailCo2e.Ma, file=Filename )
	rm( TruckFuel.MaFt, BusFuel.MaFt, TruckCo2e.Ma, BusCo2e.Ma, RailPower.Ma, RailCo2e.Ma )


  # Clean up
  gc()
