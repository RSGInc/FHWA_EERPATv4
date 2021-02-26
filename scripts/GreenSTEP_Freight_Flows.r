#========================
#GreenSTEP_FAF_Flows.r
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

#This script takes trade-partners output and FAF flow tables and assigns faf flows to supplier/buyer pairs table
  
# Iterate by year. A faf flow table with trade partners is created for each year.

# Read in inputs
FAFdata        <- file.path (ModelDir, "freight_forecasts", paste0("freight_flows_", yr, ".rds"))
FAFFlows       <- readRDS  (file = FAFdata)

# Load the trade-partners table
load(file.path (RunYearDir, "TradePartners.RData"))

# Simulate the freight tonnage between trade partners
# Aggregate FAF flow data over mode to get the total tonnage/value between two FAFZones
FAFFlows.AggregatedMode <- FAFFlows[, .(Tons = sum(Tons), Value = sum(Value)),
                                    by = .(Origin.Domestic, Destination.Domestic, SCTG, Movement.Type)] # TODO: Domestic vs. Foreign
                                                                                                        
# Look up the total tonnage moving between FAF zones
TradePartners[FAFFlows.AggregatedMode[, .(FAFZone.Buyer = Destination.Domestic,
                                          FAFZone.Seller = Origin.Domestic,
                                          SCTG, Tons_zz = Tons)],
              Tons_zz := i.Tons_zz,
              on = c("FAFZone.Buyer", "FAFZone.Seller", "SCTG")]

# Look up buyer/seller employee counts
TradePartners <- merge(TradePartners, NationalFirms[, .(BusID.Buyer = BusID, Employees.Buyer = NumEmp)],
                       by = "BusID.Buyer", all.x = TRUE)
TradePartners <- merge(TradePartners, NationalFirms[, .(BusID.Seller = BusID, Employees.Seller = NumEmp)],
                       by = "BusID.Seller", all.x = TRUE)

# Assign 10 employees to seller/buyers with NA)
TradePartners[is.na(Employees.Buyer), Employees.Buyer := 10]
TradePartners[is.na(Employees.Seller), Employees.Seller := 10]

# External-to-Internal (XI): Calculate the tonnage between trade partners
TradePartners[Movement.Type == "XI", Tons := Tons_zz * Employees.Buyer / sum(Employees.Buyer),
              by = .(FAFZone.Buyer, FAFZone.Seller, SCTG)]

# Internal-to-External (IX): Calculate the tonnage between trade partners
TradePartners[Movement.Type == "IX", Tons := Tons_zz * Employees.Seller / sum(Employees.Seller),
              by = .(FAFZone.Buyer, FAFZone.Seller, SCTG)]

# Internal-to-Internal (II): Calculate the tonnage between trade partners
TradePartners[Movement.Type == "II", Tons := Tons_zz * Employees.Buyer * Employees.Seller / sum(Employees.Buyer * Employees.Seller),
              by = .(FAFZone.Buyer, FAFZone.Seller, SCTG)]

TradePartners[, c("Tons_zz", "Employees.Buyer", "Employees.Seller") := NULL]
setkey(TradePartners, BusID.Seller, BusID.Buyer, SCTG)

# Drop any pairs that were allocated zero tons
TradePartners <- TradePartners[Tons > 0]

# Scale tonnages between pairs to the total tonnage from the FAFFlows table
# This is done by SCTG, but could be done across other dimensions such as O/D pairs
TradePartners[FAFFlows[, .(Tons = sum(Tons)), by = SCTG],
              Tons.Actual := i.Tons, on = "SCTG"]
TradePartners[, Tons := Tons/sum(Tons)*Tons.Actual, by = SCTG]
TradePartners[, Tons.Actual := NULL]

save(TradePartners, file = file.path(RunYearDir, "TradePartners.RData"))
