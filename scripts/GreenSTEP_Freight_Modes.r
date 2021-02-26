######################################
## GreenSTEP_ModeChoice_VehicleSize.r
######################################
#Author: Kaveh Shabani
#Contact: kaveh.shabani@rsginc.com
#Version: 4.0
#Date: June 2016
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

#Description
#===========

#This script takes trade-partners output and FAF flow tables and assigns faf flows to supplier/buyer pairs table
    
# Read in inputs (some preprocessed and some are outputs of previous steps)
TruckShare     <- fread(file.path (ModelDir, "truck_share.csv"))
TruckPayload   <- fread(file.path (ModelDir, "truck_payloads.csv"))
TruckEmpty     <- fread(file.path (ModelDir, "truck_percent_empty.csv"))

# Load the trade-partners table
load(file.path (RunYearDir, "TradePartners.RData"))

# Select mode by which tonnage moves between freight partners

# Interstate trips are assigned to modes (air, rail, truck, etc.)
# Intrastate trips are assigned to one of two truck types: heavy and medium

# The general approach is as follows.
# 1. Calculate mode shares by FAF zone origin/destination and SCTG
# 2. Apply this share to the assigned tonnages for II, XI, and IX movements
#   2a. II movements allocate truck tonnage proportionally to medium and heavy trucks
#   2b. IX and XI movements allocate the tonnage proportionally to modes

# Calculate mode shares by Tonnage
# Mode shares are calculated by commodity and tons
ModeShares <- FAFFlows[, .(Tons = sum(Tons)), by = .(Origin.Domestic, Destination.Domestic, SCTG, Mode.Domestic)]
ModeShares[, Share := Tons/sum(Tons), by = .(Origin.Domestic, Destination.Domestic, SCTG)]

# Split IX/XI off from II to perform the calculations separately (long distance/interstate vs. intrastate movements)
TradePartners.IX.XI <- TradePartners[Movement.Type %in% c("IX", "XI")]
TradePartners.II    <- TradePartners[Movement.Type == "II"]

# Select mode for IX and XI movements: keep all modes tonnages for a pair
TradePartners.IX.XI <- merge(TradePartners.IX.XI,
                            ModeShares[, .(FAFZone.Seller = Origin.Domestic,
                                           FAFZone.Buyer = Destination.Domestic,
                                           SCTG, Mode = Mode.Domestic, ModeShare = Share)],
                            by = c("FAFZone.Seller", "FAFZone.Buyer", "SCTG"),allow.cartesian = TRUE)

TradePartners.IX.XI[, Tons := Tons * ModeShare]
TradePartners.IX.XI[, ModeShare := NULL]

# Assume all trucks for interstate movements are heavy trucks
TradePartners.IX.XI[Mode == "Truck", Mode := "Heavy"]

# Separate truck mode from the rest of the modes
TradePartners.IX.XI.Truck  <- TradePartners.IX.XI[Mode=="Heavy"]
TradePartners.IX.XI.Other  <- TradePartners.IX.XI[Mode!="Heavy"]

# Convert truck tons to truckloads (using payload factors preprocessed from VIUS data)
TradePartners.IX.XI.Truck[TruckPayload, Payload := i.Payload, on = "SCTG"]
TradePartners.IX.XI.Truck[, Truckloads := Tons / Payload] 
TradePartners.IX.XI.Truck[, Payload := NULL]

# combine the two tables again to get all modes
TradePartners.IX.XI <- rbind(TradePartners.IX.XI.Other, TradePartners.IX.XI.Truck, fill=TRUE)

# Select mode for II movements
TradePartners.II <- merge(TradePartners.II, ModeShares[, .(FAFZone.Seller = Origin.Domestic,
                                                         FAFZone.Buyer = Destination.Domestic,
                                                         SCTG, Mode = Mode.Domestic, ModeShare = Share)],
                         by = c("FAFZone.Seller", "FAFZone.Buyer", "SCTG"),allow.cartesian = TRUE)

TradePartners.II[, Tons := Tons * ModeShare]

# Separate truck mode from the rest of the modes
TradePartners.II.Truck  <- TradePartners.II[Mode=="Truck"]
TradePartners.II.Other  <- TradePartners.II[Mode!="Truck"]

# Assign truck type (medium or heavy) (using truck type shares preprocessed from VIUS data)
TruckShare <- melt(TruckShare[,.(SCTG, Medium, Heavy)], id.vars = "SCTG", variable.name = "Type", value.name = "Percent")

TradePartners.II.Truck  <- merge (TradePartners.II.Truck, TruckShare[, .(SCTG,Type,Percent )], by = c("SCTG"),allow.cartesian = TRUE)
TradePartners.II.Truck[, Tons := Tons * Percent]
TradePartners.II.Truck[, c("Percent", "Mode","ModeShare") := NULL]
setnames(TradePartners.II.Truck, old = "Type", new = "Mode")

# Convert truck tons to truckloads
TradePartners.II.Truck[TruckPayload, Payload := i.Payload, on = "SCTG"]
TradePartners.II.Truck[, Truckloads := Tons / Payload] 
TradePartners.II.Truck[, Payload := NULL]

# combine the two tables again to get all modes
TradePartners.II <- rbind(TradePartners.II.Other, TradePartners.II.Truck, fill=TRUE)
TradePartners.II[, ModeShare := NULL]

# Combine and results
FreightFlows <- rbind(TradePartners.II, TradePartners.IX.XI)
setkey(FreightFlows, BusID.Seller, BusID.Buyer, SCTG)

# Add empty truck trips (using percent empties preprocessed from VIUS data)
# In the VIUS survey, the driver was asked what % of time did they drive empty. This is the % of total trips. To calculate number of empty trucks:
# A + B = T      (A: Loaded truck trips, B: Empty truck trips, T: Total trips)
# B = x * T      (x: % of total trips driven empty)
# A + (x * T) = T    >>>  T = A / (1 - x)  >>> B = x * T  >>> B = x * (A / (1 - x) )
FreightFlows[TruckEmpty, PctEmpty := i.PctEmpty, on = "SCTG"]
FreightFlows[, EmptyTrucks := Truckloads * (PctEmpty/(1-PctEmpty))]
FreightFlows[, TotalTrucks := Truckloads + EmptyTrucks]
FreightFlows[, PctEmpty := NULL]

########################################################################################################################################
# Add average miles traveled for interstate and intrastate truck movements (to be able to calculate VMT)
# Intracounty: ORNL intracounty distance is used. Alternative approach could be to use average daily miles traveled
# for "Movement of Goods" from "Accounting for Commercial Vehicles in Urban Transportation Models" FHWA report.
# Interstate: Average distance from county centroid to surrounding state polygons.

# Lookup business IDs and county FIPS for Businesses
FreightFlows[NationalFirms[, .(BusID.Seller = BusID, County)], County.Seller := i.County, on = "BusID.Seller"] 
FreightFlows[NationalFirms[, .(BusID.Buyer  = BusID, County)], County.Buyer := i.County, on = "BusID.Buyer"] 

hwydist_county2county <- merge(hwydist_county2county,
                               CountyFIPSCorr[, .(County.Origin = County, FIPS.Origin = FIPS)],
                               by = "FIPS.Origin")

hwydist_county2county <- merge(hwydist_county2county,
                               CountyFIPSCorr[, .(County.Destination = County, FIPS.Destination = FIPS)],
                               by = "FIPS.Destination")

# Lookup distance and FIPS by buyer/seller county
FreightFlows[hwydist_county2county[, .(County.Buyer  = County.Destination, FIPS = FIPS.Destination)],
                                       FIPS.Buyer := i.FIPS, on = "County.Buyer"]
FreightFlows[hwydist_county2county[, .(County.Seller = County.Origin,      FIPS = FIPS.Origin)],
                                       FIPS.Seller := i.FIPS, on = "County.Seller"]

FreightFlows[hwydist_county2county[, .(FIPS.Seller = FIPS.Origin, FIPS.Buyer = FIPS.Destination, Distance)],
                                        Distance := i.Distance, on = c("FIPS.Seller", "FIPS.Buyer")]

#########################################################################################
# Calculate average distance for the portion of interstate trips that are inside the state

USCountyShape <- readOGR(ModelDir, "tl_2015_us_county")
USStateShape  <- readOGR(ModelDir, "tl_2015_us_state")

#Isolate the simulated state
StateFIPS <- CountyFIPSCorr[, unique(StateFIPS)]
CountyShape     <- USCountyShape[as.integer(as.character(USCountyShape$STATEFP)) == StateFIPS,]
StateShape      <- USStateShape[as.integer(as.character(USStateShape$STATEFP)) == StateFIPS,]
OtherStateShape <- USStateShape[as.integer(as.character(USStateShape$STATEFP)) != StateFIPS,]

# Find out adjacent states that share a border with the state
SurroundingStates     <- gTouches(OtherStateShape, StateShape, byid = TRUE)
SurroundingStates     <- data.table(as.vector(SurroundingStates), OtherStateShape@data$STATEFP)
OtherStateShape@data  <- merge(OtherStateShape@data, SurroundingStates[, .(STATEFP = V2, Test = V1)], by = "STATEFP")
AdjacentStates        <- OtherStateShape[OtherStateShape$Test == "TRUE",]

# Find correct EPSG projection for the state (http://spatialreference.org/ref/epsg/2248/)
# TODO: This projection (epsg 2248) was manually searched and found. To make this generic for all states, the code needs revision.
CountyShape    <- spTransform(CountyShape,CRS("+init=epsg:2248"))
AdjacentStates <- spTransform(AdjacentStates,CRS("+init=epsg:2248"))

#create a points layer from the counties
CountyShape <- SpatialPointsDataFrame(coordinates(CountyShape),
                                      data.frame(CountyShape),
                                      bbox=bbox(CountyShape),
                                      proj4string=CRS(proj4string(CountyShape)))

# Calculate closest distance from county centroid to states boundary
# TODO: This calculates nearest distance to all surrounding states boundary
interstate_distances <- gDistance(CountyShape, as(AdjacentStates, "SpatialLines"), byid = TRUE) 
interstate_distances <- data.table(as.vector(interstate_distances), CountyShape@data$STATEFP, CountyShape@data$COUNTYFP)

setnames(interstate_distances, c("Distance", "STATEFP", "COUNTYFP"))
interstate_distances[, CountyFIPS := paste0(STATEFP, COUNTYFP)]
interstate_distances[, c("STATEFP","COUNTYFP") := NULL]
setkey(interstate_distances, CountyFIPS, Distance)
interstate_distances[, Distance := Distance * 0.000189394] # convert ft to miles (the epsg projection is in ft)

# calculate average distance to surrounding states boundary
interstate_distances    <-  interstate_distances[,.(AvgDist = mean(Distance)), by = CountyFIPS]

########################################################################################################################################
# Separate IX and XI movements to assign interstate distances (the portion inside state)
FreightFlows.IX  <- FreightFlows[Movement.Type %in% c("IX")]
FreightFlows.XI  <- FreightFlows[Movement.Type %in% c("XI")]
FreightFlows.II  <- FreightFlows[Movement.Type %in% c("II")]

FreightFlows.IX[, c("Distance") := NULL]
FreightFlows.XI[, c("Distance") := NULL]

# Separate IX and XI movements to assign interstate distances (the portion inside state)
FreightFlows.IX[interstate_distances[, .(FIPS.Seller = as.integer(CountyFIPS), AvgDist)], Distance := AvgDist, on = c("FIPS.Seller")]
FreightFlows.XI[interstate_distances[, .(FIPS.Buyer  = as.integer(CountyFIPS), AvgDist)], Distance := AvgDist, on = c("FIPS.Buyer")]

# combine the two tables again to get all modes
FreightFlows       <- rbind(FreightFlows.IX, FreightFlows.XI, FreightFlows.II, fill=TRUE)
rm(FreightFlows.XI, FreightFlows.IX, FreightFlows.II)

# Calculate VMT
FreightFlows[, VMT := TotalTrucks * Distance]

# Save the output table
save(FreightFlows, file = file.path(RunYearDir, "FreightFlows.RData"))
