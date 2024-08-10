######################
# CONFIGURATION FILE #
######################


# 2. Calibrate baseline for comparison (BF-BOF)
source(file.path ("code","configuration", "calibration", "company", "companyconfig.R"))
source(file.path("code", "configuration", "calibration", "technology", paste("techcalib_", baseline,"_", baseline_SA, ".R", sep = "")))
source(file.path("code","configuration", "calibration", "macroeconomic", paste("macrocalib_", baseline, ".R", sep = "")))

# 3. Calibrate scenario 
source(file.path("code", "configuration", "calibration", "technology", paste("techcalib_", scenario_label,"_", scenario_SA, ".R", sep = "")))
source(file.path("code","configuration", "calibration", "macroeconomic", paste("macrocalib_", scenario_label,"_", scenario_SA, ".R", sep = "")))

