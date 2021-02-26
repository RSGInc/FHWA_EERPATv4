######################################
## GreenSTEP_Trade_Partners.r
######################################
#Author: Kaveh Shabani
#Contact: kaveh.shabani@rsginc.com
#Version: 4.0
#Date: 2016-06-07
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

#Description
#===========

#This script takes firm synthesis output and BEA Input/Output tables and adds supplier/buyer categorization to the firm synthesis table

# Read in inputs
set.seed(10)

hwydist_county2county <- fread(file.path (ModelDir, "dist_county2county.csv"))
hwydist_faf2county    <- fread(file.path (ModelDir, "dist_faf2county.csv"))
NAICSSCTGCorr         <- fread(file.path (ModelDir, "corresp_naics6_n6io_sctg.csv"))
CountyFIPSCorr        <- fread(file.path (ModelDir, "county_fips.csv"))
StateFIPSCorr         <- fread(file.path (ModelDir, "faf_state_lookup.csv"))
CommodityIO           <- readRDS(file = file.path (ModelDir, "io_table.rds"))
FAFFlows              <- readRDS(file = file.path (ModelDir, paste0("/freight_forecasts/freight_flows_", yr, ".rds")))

ProValPctThres = 0.80 # TODO: Is this an argument that could be user-defined? Might need to lower the % to avoid huge tables in the code.

#Get Inside State FAF Zones
StateFAFZones <- StateFIPSCorr[State == state]$FAFZone

# Melt the distance matrices to use later in the code
hwydist_county2county <- melt(hwydist_county2county, id.vars = "County", variable.name = "County.Destination", value.name = "Distance")
setnames(hwydist_county2county, old = "County", new = "County.Origin")
hwydist_faf2county    <- melt(hwydist_faf2county, id.vars = "FAFZone", variable.name = "County", value.name = "Distance")

# Merge to add FIPS codes for counties
hwydist_county2county[CountyFIPSCorr[, .(County.Origin = County, FIPS)], FIPS.Origin := i.FIPS, on = "County.Origin"]
hwydist_county2county[CountyFIPSCorr[, .(County.Destination = County, FIPS)], FIPS.Destination := i.FIPS, on = "County.Destination"]
hwydist_county2county[, c("County.Origin","County.Destination") := NULL]
hwydist_faf2county[CountyFIPSCorr[,.(County, FIPS)], FIPS := i.FIPS, on = "County"]
hwydist_faf2county[, County := NULL]
hwydist_faf2county[, FAFZone := as.integer(FAFZone)]

# Load the firms table
Firms <- load(file.path(ModelDir, paste0("Firms", yr, ".RData")))
NationalFirms <- get(Firms)

#Create buyer/seller pairs
#===================================================================================

# Create buyer-seller pairings
# - FAF zones outside of the model region are treated as "firms"  # TODO: Need to check if this is true in the EERPAT model design
# - All firms that require commodity inputs (buyers) are matched up with
#   other firms who produce those commodities (sellers)

# Look up all of the inputs needed to make a product
# and only keep the top ProValPctThres% of inputs
CommodityIO <- CommodityIO[ProducerValue > 0]
setorder(CommodityIO, NAICSio.Use, -ProducerValue)
CommodityIO[, CumPct := cumsum(ProducerValue)/sum(ProducerValue), by = NAICSio.Use]
CommodityIO <- CommodityIO[CumPct < ProValPctThres, !c("CumPct", "ProducerValue"), with = FALSE]

# Merge in the I/O NAICS codes and SCTG codes
# This removes a few businesses with unknown NAICS codes
NationalFirms <- merge(NationalFirms, NAICSSCTGCorr[, .(NAICS = naics, NAICS6io.Output = NAICS6_IO, SCTG)], by = "NAICS") 
NationalFirms[, NAICS4 := as.integer(substr(x = NAICS, start = 1, stop = 4))]
NationalFirms[, NAICS2 := as.integer(substr(x = NAICS, start = 1, stop = 2))]
NationalFirms[, temprand := runif(.N)]

NationalFirms[NAICS == 211111, SCTG := c(16L, 19L)     [1 + findInterval(temprand,   0.45       )]] # Crude Petroleum and Natural Gas Extraction: Crude petroleum; Coal and petroleum products, n.e.c. 
NationalFirms[NAICS == 324110, SCTG := c(17L, 18L, 19L)[1 + findInterval(temprand, c(0.25, 0.50))]] # Petroleum Refineries: Gasoline and aviation turbine fuel; Fuel oils; Coal and petroleum products, n.e.c.

NationalFirms[NAICS4 == 4233, SCTG := c(10L, 11L, 12L, 25L, 26L)[1 + findInterval(temprand, c(0.10, 0.20, 0.80, 0.90))]] # Lumber and Other Construction Materials Merchant Wholesalers
NationalFirms[NAICS4 == 4235, SCTG := c(13L, 14L, 31L, 32L)     [1 + findInterval(temprand, c(0.25, 0.50, 0.75      ))]] # Metal and Mineral (except Petroleum) Merchant Wholesalers
NationalFirms[NAICS4 == 4247, SCTG := c(16L, 17L, 18L, 19L)     [1 + findInterval(temprand, c(0.25, 0.50, 0.75      ))]] # Petroleum and Petroleum Products Merchant Wholesalers
NationalFirms[NAICS4 == 4246, SCTG := c(20L, 21L, 22L, 23L)     [1 + findInterval(temprand, c(0.25, 0.50, 0.75      ))]] # Chemical and Allied Products Merchant Wholesalers
NationalFirms[NAICS4 == 4245, SCTG := c(1L,  2L,  3L,  4L)      [1 + findInterval(temprand, c(0.25, 0.50, 0.75      ))]] # Farm Product Raw Material Merchant Wholesalers
NationalFirms[NAICS4 == 4244, SCTG := c(5L,  6L,  7L,  9L)      [1 + findInterval(temprand, c(0.25, 0.50, 0.75      ))]] # Grocery and Related Product Wholesalers
NationalFirms[NAICS4 == 4241, SCTG := c(27L, 28L, 29L)          [1 + findInterval(temprand, c(0.33, 0.67            ))]] # Paper and Paper Product Merchant Wholesalers 
NationalFirms[NAICS4 == 4237, SCTG := c(15L, 33L)               [1 + findInterval(temprand,   0.50                   )]] # Hardware, and Plumbing and Heating Equipment and Supplies Merchant Wholesalers
NationalFirms[NAICS4 == 4251, SCTG := c(35L, 38L)               [1 + findInterval(temprand,   0.50                   )]] # Wholesale Electronic Markets and Agents and Brokers
NationalFirms[NAICS4 == 4236, SCTG := c(35L, 38L)               [1 + findInterval(temprand,   0.50                   )]] # Electrical and Electronic Goods Merchant Wholesalers
NationalFirms[NAICS4 == 4231, SCTG := c(36L, 37L)               [1 + findInterval(temprand,   0.50                   )]] # Motor Vehicle and Motor Vehicle Parts and Supplies Merchant Wholesalers
NationalFirms[NAICS4 == 4248, SCTG :=   8L ] # Beer, Wine, and Distilled Alcoholic Beverage Merchant Wholesalers
NationalFirms[NAICS4 == 4242, SCTG :=   21L] # Drugs and Druggists Sundries Merchant Wholesalers
NationalFirms[NAICS4 == 4234, SCTG :=   24L] # Professional and Commercial Equipment and Supplies Merchant Wholesalers
NationalFirms[NAICS4 == 4243, SCTG :=   30L] # Apparel, Piece Goods, and Notions Merchant Wholesalers
NationalFirms[NAICS4 == 4238, SCTG :=   34L] # Machinery, Equipment, and Supplies Merchant Wholesalers
NationalFirms[NAICS4 == 4232, SCTG :=   39L] # Furniture and Home Furnishing Merchant Wholesalers
NationalFirms[NAICS4 == 4239, SCTG :=   40L] # Miscellaneous Durable Goods Merchant Wholesalers
NationalFirms[NAICS4 == 4249, SCTG :=   40L] # Miscellaneous Nondurable Goods Merchant Wholesalers

NationalFirms[NAICS2 == 42, NAICS6io.Output := paste0(NAICS4, "00")]

####################################################################################################
## Create a sample of firms based on the following rules to form the basis of the producer-consumer pairs to be simulated:
# 1. Keep all individual businesses in the desired state.
# 2. Keep all large businesses throughout the U.S
# 3. Keep those identified as "MustKeep"
# 4. Randomly sample an additional small percentage (currently set as 5%)

# Create a flag to make sure at least one firm is used as a buyer for each zone and commodity combination
NationalFirms[NationalFirms[, .(index = sample(x = .I, size = 1)), by = .(FAFZone, County, NAICS, NAICS6io.Output, SCTG, NumEmp)][, index], MustKeep := 1L]
NationalFirms[is.na(MustKeep), MustKeep := 0L]

#Create the sample firms
NationalFirms[, temprand := runif(.N)]
NationalFirms <- NationalFirms[temprand > 0.95 | NumEmp >= 750 | MustKeep == 1] # TODO: The sample % or firm size could be modified by users

# Remove extra fields
NationalFirms[, c("MustKeep", "temprand") := NULL]

# Lookup county FIPS to the table
NationalFirms[CountyFIPSCorr[, .(County, FIPS)], FIPS := i.FIPS, on = "County"]

# Create lists of buyer and supplier firms within the model region
# Only include sellers of transported goods
Sellers.County <- NationalFirms[SCTG>0, .(BusID, FAFZone, County, FIPS, SCTG.Output = SCTG, NAICS6io.Output, NumEmp)]
Buyers.County  <- NationalFirms[,       .(BusID, FAFZone, County, FIPS, NAICS6io.Output, NumEmp)]

# Look up the required inputs' NAICS for buyers within the model region
Buyers.County <- merge(Buyers.County, CommodityIO[, .(NAICS6io.Output = NAICSio.Use, NAICS6io.Input = NAICSio.Make)],
                       by = "NAICS6io.Output", allow.cartesian = TRUE) # Note: With full firm table this merge uses up to 8 Gb of RAM!

# Look up the required inputs' SCTG for buyers
# Some NAICS can produce more than one SCTG, so need to simulate exactly one
Buyers.County <- merge(Buyers.County, unique(NAICSSCTGCorr[, .(NAICS6io.Input = NAICS6_IO, SCTG.Input = SCTG, Prop)]),
                       by = "NAICS6io.Input", allow.cartesian = TRUE) # Note: With full firm table this merge uses ~14 Gb of RAM! takes ~30 sec

Buyers.County[, Prop := if(all(Prop == 0)) {1} else {Prop}, by = .(BusID, NAICS6io.Input)] 
Buyers.County[, KeepSCTG := .I == .I[sample(length(.I), size = 1, prob = Prop)], by = .(BusID, NAICS6io.Input)] # Notes: With full firm table this merge takes ~ 300 sec
Buyers.County <- Buyers.County[KeepSCTG == TRUE, !c("KeepSCTG", "Prop"), with = FALSE]

# Only keep SCTG inputs that are transported
Buyers.County <- Buyers.County[!is.na(SCTG.Input) & SCTG.Input != 0]

# Aggregate up to the SCTG-level regarding buyer inputs
Buyers.County <- unique(Buyers.County[,  !"NAICS6io.Input",  with = FALSE])

# Aggregate FAF flow data over mode to determine possible FAF zone sellers
FAFFlows.AggregatedMode <- FAFFlows[, .(Tons = sum(Tons), Value = sum(Value)),
                                    by = .(Origin.Domestic, Destination.Domestic, SCTG, Movement.Type)] 
                                                                                                        
# Match buyers with sellers
Buyers.County <- merge(Buyers.County, FAFFlows.AggregatedMode[, .(FAFZone = Destination.Domestic, FAFZone.Seller = Origin.Domestic,
                       SCTG.Input = SCTG, W_z_p = Value, Movement.Type)],
                       by = c("FAFZone", "SCTG.Input"), allow.cartesian = TRUE)

# Select a seller FAF zone for each firm's SCTG input needs, weighted by freight value (W_z_p)
Buyers.County[, SelectedSeller := .I == .I[sample(length(.I), size = 1, prob = W_z_p)], by = .(BusID, SCTG.Input)]
Buyers.County <- Buyers.County[SelectedSeller == TRUE, !c("W_z_p", "SelectedSeller"), with = FALSE]

# Match buyers with specific sellers
Buyers.County <- merge(Buyers.County,
                       Sellers.County[, .(BusID.Seller = BusID, FAFZone.Seller = FAFZone, SCTG.Input = SCTG.Output, Employees.Seller = NumEmp)],
                       by = c("FAFZone.Seller", "SCTG.Input"), allow.cartesian = TRUE, all.x = TRUE)

Buyers.County <- Buyers.County [!(is.na(BusID.Seller))]

# Don't allow firms to sell to themselves
Buyers.County <- Buyers.County[(BusID != BusID.Seller) | is.na(BusID.Seller)]

# Pick exactly one seller from the model region for each SCTG input a buyer needs
# if they were matched with the model region seller pool
# First, look up the distance between buyer and seller
Buyers.County[NationalFirms[, .(BusID.Seller = BusID, FIPS)], FIPS.Seller := i.FIPS, on = "BusID.Seller"]

Buyers.County[hwydist_county2county[, .(FIPS = FIPS.Destination, FIPS.Seller = FIPS.Origin, Distance)],
              CtoC_Distance := i.Distance, on = c("FIPS", "FIPS.Seller")]

Buyers.County[hwydist_faf2county[, .(FIPS, FAFZone.Seller = FAFZone, Distance)],
              FtoC_Distance := i.Distance, on = c("FIPS", "FAFZone.Seller")] # XI Movements

Buyers.County[hwydist_faf2county[, .(FIPS.Seller = FIPS, FAFZone, Distance)],
              FtoC_Distance := i.Distance, on = c("FIPS.Seller", "FAFZone")] # IX Movements

# Make sure to keep only pairs with one end in the state
isTRUE(nrow(Buyers.County) == nrow(Buyers.County [ (FAFZone.Seller %in% StateFAFZones) | (FAFZone %in% StateFAFZones) ]))

Buyers.County <- Buyers.County [ (FAFZone.Seller %in% StateFAFZones) | (FAFZone %in% StateFAFZones) ]

# Remove cases with no lookup distance (these should include impossible routes (e.g. HI to CA), etc.)
isTRUE(nrow(Buyers.County) == nrow(Buyers.County[!(is.na(CtoC_Distance) & is.na(FtoC_Distance))]))

Buyers.County <- Buyers.County[!(is.na(CtoC_Distance) & is.na(FtoC_Distance))]

# Select sellers for buyers in each movement type
Beta_d <- 25
Buyers.County[Movement.Type == "II", SellerSelected := .I == .I[sample(length(.I), size = 1,
              prob = Employees.Seller * exp(-CtoC_Distance/Beta_d))],
              by = .(BusID, SCTG.Input)]

Buyers.County[Movement.Type != "II",
              SellerSelected := .I == .I[sample(length(.I), size = 1,
              prob = Employees.Seller * exp(-FtoC_Distance/Beta_d))],
              by = .(BusID, SCTG.Input)]

# Create a table of trade partners within the model region
TradePartners.Internal <- Buyers.County[SellerSelected == TRUE & Movement.Type == "II",
                                        .(BusID.Buyer = BusID, BusID.Seller,
                                          FAFZone.Buyer = FAFZone, FAFZone.Seller,
                                          SCTG = SCTG.Input, Movement.Type)]

# Create a table of trade partners where the seller is out of the model region
TradePartners.ExternalSeller <- Buyers.County[SellerSelected == TRUE & Movement.Type == "XI",
                                              .(BusID.Buyer = BusID, FAFZone.Buyer = FAFZone,
                                                FAFZone.Seller, SCTG = SCTG.Input, Movement.Type)]

# Match sellers within the model region with buyers at the FAF zone level
# outside of the model region
set.seed(1987)
Sellers.County <- merge(Sellers.County,
                        FAFFlows.AggregatedMode[Movement.Type == "IX",
                                                .(FAFZone =  Origin.Domestic,
                                                  FAFZone.Buyer =  Destination.Domestic,
                                                  SCTG.Output = SCTG, Movement.Type)],
                        by = c("FAFZone", "SCTG.Output"), allow.cartesian = TRUE)

# For each buyer and each commodity they need to select a
# single seller firm within the model region
Sellers.County[, SellerSelected := .I == .I[sample(length(.I), size = 1, prob = NumEmp)],
               by = .(FAFZone.Buyer, SCTG.Output)]

# Create a table of trade partners where the buyer is out of the model region
TradePartners.ExternalBuyer <- Sellers.County[SellerSelected == TRUE & Movement.Type == "IX",
                                              .(FAFZone.Buyer, BusID.Seller = BusID,
                                                FAFZone.Seller = FAFZone, SCTG = SCTG.Output, Movement.Type)]

# Create a final table of trade partners
TradePartners <- rbind(TradePartners.Internal, TradePartners.ExternalBuyer, TradePartners.ExternalSeller, fill = TRUE)
setkey(TradePartners, BusID.Seller, BusID.Buyer)
rm(TradePartners.ExternalBuyer, TradePartners.ExternalSeller, TradePartners.Internal)

save(TradePartners, file = file.path(RunYearDir, "TradePartners.RData"))
