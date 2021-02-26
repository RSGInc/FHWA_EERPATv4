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

#The GreenSTEP_Pop_Road.r module produces POPULATION AND LANE-MILE TABULATIONS FOR ALL YEARS

#============================================================
#CALCULATE POPULATION AND LANE-MILE TABULATIONS FOR ALL YEARS
#============================================================

#Population is tabulated by county and year for all years so that population growth rates may be calculated to use to calculate factors that are based on growth rates. Annual growth of freeway and arterial lane miles is calculated so that the annual construction costs may be calculated. This is used to calculate whether fuel taxes are sufficient to cover costs and how much VMT taxes should be raised to cover costs.

#Tabulate population by county and year. Do this only if it has not been done already.
#=====================================================================================
if( !file.exists( "outputs/Pop.CoYr.RData" ) ){ 
Pop.CoYr <- array( 0, dim=c( length(Co), length(Yr) ), dimnames=list( Co, Yr ) )
for( yr in Yr ) {
  # Load the synthetic population file for the year
  setwd(ModelDir)
  PopFileName <- paste( "Hsld", yr, ".RData", sep="" )
  Pop_ <- assignLoad( PopFileName )
  rm( PopFileName )
  setwd( RunDir )
  # Tabulate total population by county
  Pop.CoYr[ , yr ] <- unlist( lapply( Pop_, sum ) )[ Co ]
  rm( Pop_ )
}
save( Pop.CoYr, file="outputs/Pop.CoYr.RData" )
} else {
	Pop.CoYr <- assignLoad( "outputs/Pop.CoYr.RData" )
}

#Calculate annual increase in metropolitan area lane miles
#=========================================================

# Calculate metropolitan area population
PopGrowth.CoYr <- sweep( Pop.CoYr, 1, Pop.CoYr[,BaseYear], "-" )

MetropolitanPop.CoYr <- Pop.CoYr * 0
MetropolitanPopGrowth.CoYr <- Pop.CoYr * 0
for( yr in Yr ) {
  if (yr <= BaseYear){
    MetropolitanPop.CoYr[,yr] <- Pop.CoYr[,yr] * UrbRurPopProp.CoDt[,"Metropolitan"]
  } else {
    MetropolitanPopGrowth.CoYr[,yr] <- PopGrowth.CoYr[,yr] * UrbRurGrowthSplit.CoDt[,"Metropolitan"]
    MetropolitanPop.CoYr[,yr] <- MetropolitanPop.CoYr[,BaseYear] + MetropolitanPopGrowth.CoYr[,yr]
  }
  
}

MetropolitanPop.MaYr <- apply( MetropolitanPop.CoYr, 2, function(x) {
    tapply( x, CountyGroups..$Msa, sum ) } )
if (length(Ma)==1) MetropolitanPop.MaYr <- matrix(data=MetropolitanPop.MaYr, nrow=length(Ma),ncol=length(Yr),byrow=TRUE,dimnames=list(Ma,Yr))
MetropolitanPop.MaYr <- MetropolitanPop.MaYr[Ma,,drop = FALSE]

MetropolitanPopChange.MaYr <- sweep( MetropolitanPop.MaYr, 1, MetropolitanPop.MaYr[,BaseYear], "/" ) - 1
rm( PopGrowth.CoYr, MetropolitanPop.CoYr, MetropolitanPopGrowth.CoYr )

# Calculate the annual metropolitan freeway construction cost for added lane-miles per year
FwyLnMiGrowthFactor.MaYr <- 1 + sweep( MetropolitanPopChange.MaYr[Ma,,drop = FALSE], 1, FreewayGrowth.Ma, "*" )
FwyLnMi.MaYr <- sweep( FwyLnMiGrowthFactor.MaYr, 1, BaseFwyLnMi.Ma, "*" )
FwyLnMi.Yr <- colSums( FwyLnMi.MaYr )

FwyLnMiChg.Yr <- diff( FwyLnMi.Yr )
FwyLnMiChg.Yr <- c( FwyLnMiChg.Yr[1], FwyLnMiChg.Yr )
names( FwyLnMiChg.Yr )[1] <- min(Yr)

YearChange = diff(as.numeric(Yr))
YearChange <- c( YearChange[1], YearChange )
names(YearChange) <- Yr

FwyLnMiAnnualCosts.Yr <- ( FwyLnMiChg.Yr / YearChange ) * 1000 * Costs.YrCs[Yr,"FwyLnMi"]	
rm( FwyLnMiGrowthFactor.MaYr, FwyLnMi.MaYr, FwyLnMi.Yr, FwyLnMiChg.Yr )	

# Calculate the annual metropolitan arterial construction cost for added lane-miles per year
ArtLnMiGrowthFactor.MaYr <- 1 + sweep( MetropolitanPopChange.MaYr[Ma,,drop = FALSE], 1, FreewayGrowth.Ma, "*" )
ArtLnMi.MaYr <- sweep( ArtLnMiGrowthFactor.MaYr, 1, BaseArtLnMi.Ma, "*" )
ArtLnMi.Yr <- colSums( ArtLnMi.MaYr )

ArtLnMiChg.Yr <- diff( ArtLnMi.Yr )
ArtLnMiChg.Yr <- c( ArtLnMiChg.Yr[1], ArtLnMiChg.Yr )
names( ArtLnMiChg.Yr )[1] <- min(Yr)

ArtLnMiAnnualCosts.Yr <- ( ArtLnMiChg.Yr / YearChange ) * 1000 * Costs.YrCs[Yr,"ArtLnMi"]	
rm( ArtLnMiGrowthFactor.MaYr, ArtLnMi.MaYr, ArtLnMi.Yr, ArtLnMiChg.Yr )	

# Calculate the total annual cost for adding lane miles
AnnLnMiAddCosts.Yr <- FwyLnMiAnnualCosts.Yr + ArtLnMiAnnualCosts.Yr		
rm( MetropolitanPopChange.MaYr, FwyLnMiAnnualCosts.Yr, ArtLnMiAnnualCosts.Yr )
