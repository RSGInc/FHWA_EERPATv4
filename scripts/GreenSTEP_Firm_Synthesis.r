#========================
#GreenSTEP_Firm_Synthesis.r
#========================

#Copyright 2009, Oregon Department of Transportation
#Author: Jeff Keller
#Contact: jeff.keller@rsginc.com
#Version: 4.0
#Date: 2016-07-15
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


#Description
#===========

#This script takes firm and employment projections by industry cohort, county and year and makes synthetic firms that are used in the GreenSTEP model.
#The attributes of the synthetic firms include the location, number of employees, and industry category.

#The synthetic firm data is saved as a set of .RData files in the scenario output directory.


#Create synthetic firms for each forecast year and county or FAF zone as appropriate
#===================================================================================

local( {
  
  # Load fixed inputs
  source(file.path("..", "..", "scripts", "GreenSTEP_Freight_Inputs.r"))
  
  # Iterate by year. A synthetic firm file is created for each year.
  for( yr in Yr ) {
    
    # Firm sizes (average number of employees for each firm size in CBP data)
    FirmSizes <- c(e1 = 10L, e2 = 60L, e3 = 175L, e4 = 375L, e5 = 750L, e6 = 1750L, e7 = 3750L, e8 = 7500L)
    
    # Load the firm seed file
    # These tables are created using CBP 2011 data, which use 2007 NAICS
    FirmFileName <- file.path (ModelDir, "industry_firms.rds")
    Firm..Co..NAICS <- readRDS (file = FirmFileName)
    
    # Map firms industries to census NAICS segments (CNS)
    Firm..Co..NAICS[, NAICS2 := as.integer(substr(x = NAICS, start = 1, stop = 2))]
    Firm..Co..NAICS[NAICS2 == 11, CNS := 1]
    Firm..Co..NAICS[NAICS2 == 21, CNS := 2]
    Firm..Co..NAICS[NAICS2 == 22, CNS := 3]
    Firm..Co..NAICS[NAICS2 == 23, CNS := 4]
    Firm..Co..NAICS[NAICS2 %in% 31:33, CNS := 5]
    Firm..Co..NAICS[NAICS2 == 42, CNS := 6]
    Firm..Co..NAICS[NAICS2 %in% 44:45, CNS := 7]
    Firm..Co..NAICS[NAICS2 %in% 48:49, CNS := 8]
    Firm..Co..NAICS[NAICS2 == 51, CNS := 9]
    Firm..Co..NAICS[NAICS2 == 52, CNS := 10]
    Firm..Co..NAICS[NAICS2 == 53, CNS := 11]
    Firm..Co..NAICS[NAICS2 == 54, CNS := 12]
    Firm..Co..NAICS[NAICS2 == 55, CNS := 13]
    Firm..Co..NAICS[NAICS2 == 56, CNS := 14]
    Firm..Co..NAICS[NAICS2 == 61, CNS := 15]
    Firm..Co..NAICS[NAICS2 == 62, CNS := 16]
    Firm..Co..NAICS[NAICS2 == 71, CNS := 17]
    Firm..Co..NAICS[NAICS2 == 72, CNS := 18]
    Firm..Co..NAICS[NAICS2 == 81, CNS := 19]
    Firm..Co..NAICS[NAICS2 == 92, CNS := 20]
    
    # Aggregate firm counts to CNS level
    Firm..Co..CNS <- Firm..Co..NAICS[, .(e1 = sum(e1),
                                         e2 = sum(e2),
                                         e3 = sum(e3),
                                         e4 = sum(e4),
                                         e5 = sum(e5),
                                         e6 = sum(e6),
                                         e7 = sum(e7),
                                         e8 = sum(e8)),
                                     by = .(FAFZone, County, CNS)]
    
    # Add number of employees based on firm sizes
    Firm..Co..CNS[, TotalEmp := FirmSizes[1]*e1 + FirmSizes[2]*e2 + FirmSizes[3]*e3 + FirmSizes[4]*e4 + FirmSizes[5]*e5 + FirmSizes[6]*e6 + FirmSizes[7]*e7 + FirmSizes[8]*e8]
    
    # Get just in-state firms
    StateFirms..Co..CNS <- Firm..Co..CNS[!is.na(County)]
    
    # Load the employment projections file for the year
    # These tables are created using LEHD data and are user-defined tables (current code uses 2010 LEHD data for the forecast)
    EmployFileName <- file.path(ModelDir, "employment_forecasts", paste0("employment_", yr, ".csv"))
    Employ..Co <- fread( EmployFileName )
    
    # Convert employment table to long format
    Employ..Co..CNS <- melt(Employ..Co, id.vars = "County", variable.name = "CNS", value.name = "TargetEmp")
    Employ..Co..CNS[, CNS := as.integer(substr(x = CNS, start = 4, stop = 5))]
    
    ###############################################################################################################################			
    
    # Scale firms by employment forecasts
    # There are four cases to deal with when comparing base year employment data (CBP data) and employment projections (e.g. LEHD data):
    # 1. CBP emp > 0 & LEHD emp > 0  "The below code will take care of it using "k" as the scaling factor"
    # 2. CBP emp > 0 & LEHD emp = 0  "Could be ignored if the difference is not significant" TODO: could add code to generate warning when differences are signifcant
    # 3. CBP emp = 0 & LEHD emp = 0  "Will be ignored"
    # 4. CBP emp = 0 & LEHD emp > 0  "The below code will take care of it, creating firms to be added to CBP data (using LEHD data)"
    
    ## To deal with case 4
    # Compare employment tables between base year input (CBP) and projected employment data (e.g. LEHD)
    Employment.Compare <- merge(StateFirms..Co..CNS, Employ..Co..CNS, by = c("County", "CNS"), all = TRUE)
    Employment.Compare[is.na(TotalEmp),  TotalEmp := 0]
    Employment.Compare[is.na(TargetEmp), TargetEmp := 0]
    
    # Next line eliminates case 3
    Employment.Compare <- Employment.Compare[!(TotalEmp == 0 & TargetEmp == 0)]
    
    Employment.Compare[, Employees.Difference := TargetEmp - TotalEmp]
    
    # Select empty County-Employment category combinations
    FirmsNeeded <- Employment.Compare[TotalEmp == 0]
    
    # Calculate average employment by CNS
    Employment.Avg <- StateFirms..Co..CNS[, .(Employees.Avg = mean(TotalEmp)), by = CNS]
    FirmsNeeded[Employment.Avg, Employees.Avg := i.Employees.Avg, on = "CNS", nomatch = 0]
    
    # Calculate number of firms to be sampled
    FirmsNeeded[, N := round(pmax(1, Employees.Difference/Employees.Avg))]
    FirmsNeeded <- FirmsNeeded[!is.na(N)] #TODO: This ignores any CNS with no match in the state firms data but present in the state projection table
    FirmsNeeded[Firm..Co..CNS, FAFZone := i.FAFZone, on = "County", nomatch = 0]
    FirmsNeeded[, c("Employees.Difference","Employees.Avg","TotalEmp") := NULL]
    
    # Sample the N firms needed for each County and CNS
    NewFirms <- FirmsNeeded[, .(N = sum(N), TargetEmp = sum(TargetEmp)), by = .(County, CNS)]
    
    Firm..Co..NAICS..Size <- melt(Firm..Co..NAICS, id.vars = c("FAFZone", "County", "NAICS", "NAICS2", "CNS"),
                                  measure.vars = paste0("e", 1:8), variable.name = "Size", value.name = "Count")
    Firm..Co..NAICS..Size [, ID := .I]
    
    NewFirms <- NewFirms[, .(ID = sample(x = Firm..Co..NAICS..Size[CNS == CNS.temp,ID], size = N, replace = TRUE)),
                         by = .(County, CNS.temp = CNS)] #TODO: This assumes random draw from the firms data. This could be a weightes sampling instead.
    setnames(NewFirms, old = "CNS.temp", new = "CNS")
    
    # Merge new firms table with the firm data table (with NAICS) to get information for the sampled IDs
    NewFirms <- merge(NewFirms, Firm..Co..NAICS..Size[, !c("County", "CNS"), with = FALSE], by = c("ID"))
    NewFirms[, c("Count") := 1]
    NewFirms[, c("ID") := NULL]

    # Unmelt the new firms table to be able to add to the original firms table
    NewFirms <- data.table::dcast(data = NewFirms,formula = FAFZone+County+NAICS+NAICS2+CNS~Size,fun.aggregate = sum,value.var = "Count")
    
    # Make sure that FAF zone is correct for selected Business IDs
    NewFirms[Firm..Co..CNS, FAFZone := i.FAFZone, on = "County", nomatch = 0]
    
    # Merge new firms table with the original firms data table
    Firm..Co..NAICS <- rbind(NewFirms, Firm..Co..NAICS, use.names = TRUE, fill = TRUE)
    
    # Replace "NA" in firm size counts with zero
    Firm..Co..NAICS[is.na(e1), e1:= 0]
    Firm..Co..NAICS[is.na(e2), e2:= 0]
    Firm..Co..NAICS[is.na(e3), e3:= 0]
    Firm..Co..NAICS[is.na(e4), e4:= 0]
    Firm..Co..NAICS[is.na(e5), e5:= 0]
    Firm..Co..NAICS[is.na(e6), e6:= 0]
    Firm..Co..NAICS[is.na(e7), e7:= 0]
    Firm..Co..NAICS[is.na(e8), e8:= 0]
    
    #########################################################################
    #Add synthetic records for missing coverage in CBP
    #-------------------------------------------------
    
    #check for missing making industries due to coverage of CBP
    
    #coverage of SCTG=1,2 almost entirely lacking. Add dummy "farms" in each zone
    agfirms <- data.table(expand.grid.df(data.frame(NAICS=NAICSSCTGCorr$naics[NAICSSCTGCorr$SCTG %in% c(1,2)]),
                                         unique(Firm..Co..NAICS[!is.na(County),list(FAFZone,County)])))
    agfirms[,c("e1"):=1]
    agfirms[,c(paste0("e",2:8)):=0]
    
    # Map agricultural firms industries to census NAICS segments (CNS)
    agfirms[, NAICS2 := as.integer(substr(x = NAICS, start = 1, stop = 2))]
    agfirms[NAICS2 == 11, CNS := 1]
    
    #append the ag firm data to the national firms data
    Firm..Co..NAICS <- rbind(Firm..Co..NAICS, agfirms, use.names=TRUE)

    ###############################################################################################################################			
    # With the new firm data table (original firm data + new added firms)
    # Aggregate firm counts to CNS level again
    Firm..Co..CNS <- Firm..Co..NAICS[, .(e1 = sum(e1),
                                         e2 = sum(e2),
                                         e3 = sum(e3),
                                         e4 = sum(e4),
                                         e5 = sum(e5),
                                         e6 = sum(e6),
                                         e7 = sum(e7),
                                         e8 = sum(e8)),
                                     by = .(FAFZone, County, CNS)]
    
    # Add number of employees based on firm sizes
    Firm..Co..CNS[, TotalEmp := FirmSizes[1]*e1 + FirmSizes[2]*e2 + FirmSizes[3]*e3 + FirmSizes[4]*e4 + FirmSizes[5]*e5 + FirmSizes[6]*e6 + FirmSizes[7]*e7 + FirmSizes[8]*e8]
    
    # Get just in-state firms
    StateFirms..Co..CNS <- Firm..Co..CNS[!is.na(County)]
    
    # Get target employment figures from the projection employment table
    StateFirms..Co..CNS[Employ..Co..CNS, TargetEmp := i.TargetEmp, on = c("County", "CNS"), nomatch = 0]
    StateFirms..Co..CNS[, k := TargetEmp/TotalEmp]
    StateFirms..Co..CNS[, Count := e1+e2+e3+e4+e5+e6+e7+e8]
    
    # Scale the number of firms of each size
    StateFirms..Co..CNS <- melt(StateFirms..Co..CNS, id.vars = c("County", "CNS", "k"), measure.vars = paste0("e", 1:8),
                                variable.name = "Size", value.name = "Count")
    StateFirms..Co..CNS[, Size := as.integer(substr(x = Size, start = 2, stop = 2))]
    StateFirms..Co..CNS[, AdjCount := as.integer(pmax(1,bucketRound(Count * k)))] #TODO: Think about using "round" or "ceiling" functions
    StateFirms..Co..CNS[, c("Count") := NULL]
    setnames(StateFirms..Co..CNS, old = "AdjCount", new = "Count")
    
    # Check if there are any county-CNS combinations that should have firms now, but didn't in the seed firm data
    if (StateFirms..Co..CNS[, any(is.infinite(k))]) {
      warning("Infinite employment adjustment factor detected.", immediate. = TRUE, call. = FALSE)
      StateFirms..Co..CNS <- StateFirms..Co..CNS[k < Inf]
    }
    
    # Allocate scaled firms back to 6-digit NAICS industries
    Firm..Co..NAICS..Size <- melt(Firm..Co..NAICS, id.vars = c("FAFZone", "County", "NAICS", "NAICS2", "CNS"),
                                  measure.vars = paste0("e", 1:8), variable.name = "Size", value.name = "Count")
    
    Firm..Co..NAICS..Size[, Size := as.integer(substr(x = Size, start = 2, stop = 2))]
    Firm..Co..NAICS..Size[StateFirms..Co..CNS, CNSCount := i.Count, on = c("County", "CNS", "Size")]
    Firm..Co..NAICS..Size[is.na(CNSCount), CNSCount := sum(as.integer(Count)), by = .(FAFZone, County, CNS, Size)]
    Firm..Co..NAICS..Size <- Firm..Co..NAICS..Size[CNSCount > 0]
    Firm..Co..NAICS..Size[, CNSFirmShare := Count/sum(Count), by = .(FAFZone, County, CNS, Size)] #TODO: Assumes firm share is equal for FAF, County, CNS and size combination. This assumption could be revisited.
    Firm..Co..NAICS..Size[, AdjCount := as.integer(round(CNSCount * CNSFirmShare))]
    Firm..Co..NAICS..Size <- Firm..Co..NAICS..Size[AdjCount > 0]

    # Drop extra variables
    Firm..Co..NAICS..Size[, NumEmp := FirmSizes[Size]]
    Firm..Co..NAICS..Size[, c("NAICS2", "CNS", "Count", "CNSCount", "CNSFirmShare", "Size") := NULL, with = FALSE]
    setnames(Firm..Co..NAICS..Size, old = "AdjCount", new = "Count")
    
    # Save unenumerated list of firms
    setkey(Firm..Co..NAICS..Size, FAFZone, County, NAICS)
    
    Firm..Co..NAICS..Size <- Firm..Co..NAICS..Size[rep(1:.N, times = Count)]
    Firm..Co..NAICS..Size[, c("Count") := NULL]
    Firm..Co..NAICS..Size[, BusID := .I]
    
    # Save the results for the year
    SaveFileName <- file.path( ModelDir, paste0("Firms", yr, ".RData") )
    save( Firm..Co..NAICS..Size, file=SaveFileName, compress=TRUE )
    rm( Firm..Co..NAICS..Size )
    
  }
  
} )
