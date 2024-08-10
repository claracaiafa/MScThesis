######################
# CONFIGURATION FILE #
######################


# 1. Calibrate baseline for comparison (BF-BOF)
source(file.path ("code","configuration", "calibration", "company", "companyconfig.R"))
source(file.path("code", "configuration", "calibration", "technology", paste("techcalib_", baseline,"_", baseline_SA, ".R", sep = "")))
source(file.path("code","configuration", "calibration", "macroeconomic", paste("macrocalib_", project_phase,"_", baseline_SA, ".R", sep = "")))

# 2. Calibrate scenario 
source(file.path("code", "configuration", "calibration", "technology", "techcalib_Construction.R", sep = ""))

F_list <- list(F_BR, F_NL, F_BRImpShare, F_NLImpShare, F_CNIN, F_OtherEU, F_RoW, F_USMCA)
