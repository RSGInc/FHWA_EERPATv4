
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

