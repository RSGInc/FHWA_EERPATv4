#========================
#GreenSTEP_Freight.r
#========================

#Copyright 2021, Resource Systems Group, Inc.
#Author: Colin Smith
#Contact: colin.smith@rsginc.com
#Version: 4.0
#Date: 2021-02-26
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


# Step 1: Match Buyer and Seller Firm Trade Partners ----------------------
#==========================================================================

  source(file.path(ScriptDir, "GreenSTEP_Freight_Trade_Partners.r"), local = TRUE)


# Step 2: Assign FAF Freight Flows ----------------------------------------
#==========================================================================

  source(file.path(ScriptDir, "GreenSTEP_Freight_Flows.r"), local = TRUE)

# Step 3: Simulate Mode/Vehicle -------------------------------------------
#==========================================================================

  source(file.path(ScriptDir, "GreenSTEP_Freight_Modes.r"), local = TRUE)

# Step 4: Scale Truck VMT to HPMS------------------------------------------
#==========================================================================

  source(file.path(ScriptDir, "GreenSTEP_Freight_HPMS.r"), local = TRUE)

