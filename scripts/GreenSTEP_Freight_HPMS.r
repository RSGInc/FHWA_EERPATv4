######################################
## GreenSTEP_HPMS.r
######################################
#Author: Colin Smith
#Contact: colin.smith@rsginc.com
#Version: 4.0
#Date: June 2016
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

#Description
#===========

#This script scales truck VMT to match observed HPMS VMT
    

# Calculate truck VMT by metropolitan area
#-----------------------------------------
# Calculate truck DVMT

FreightFlows.Truck <- FreightFlows[Mode %in% c("Medium", "Heavy"), .(VMT = sum(VMT)),
                                   by = Mode][order(Mode)]
TruckAnnualFactor <- 310L
TruckDvmt <- FreightFlows.Truck[["VMT"]] / TruckAnnualFactor

if( yr == BaseYear ) {
  # Compare total truck DVMT to BaseTruckVmt
  BaseTruckDvmt <- BaseTruckVmt/TruckAnnualFactor
  TruckDvmtRatio <- BaseTruckDvmt/sum(TruckDvmt, na.rm = TRUE)
  # Save the ratio for use in future years
  save(TruckDvmtRatio, file = file.path(OutputDir, "TruckDvmtRatio.RData"))
}
if( yr != BaseYear ) {
  load(file = file.path(OutputDir, "TruckDvmtRatio.RData"))
}

# Apply ratio
TruckDvmt <- TruckDvmt * TruckDvmtRatio

# Calculate proportion by truck type for later calculations
TruckProp <- TruckDvmt / sum(TruckDvmt)

# Allocate truck VMT to metropolitan areas for later congestion calculation
names(TruckDvmt) <- FreightFlows.Truck[["Mode"]]
TruckDvmt.Ma <- matrix(TruckDvmt, ncol = length(TruckDvmt),
                       nrow = length(MpoBaseDvmtParm..Ma$PropTruckDvmt), byrow = TRUE)
TruckDvmt.Ma <- TruckDvmt.Ma * MpoBaseDvmtParm..Ma$PropTruckDvmt
colnames(TruckDvmt.Ma) <- paste0(names(TruckDvmt), "Trk")

# Save the output table
save(TruckDvmt.Ma, file = file.path(RunYearDir, "TruckDvmt.Ma.RData"))
