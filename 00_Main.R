###############
# MAIN SCRIPT #
###############

# Project: Masters' Thesis in Economics
# Title: Economic impacts of green steel
# Author: Clara Rabelo Caiafa Pereira


# 1) Prepare environment
rm(list = ls())
options(scipen = 14)
source(file.path("code","packages.R"))

# 2) Run project phases

  # 1. Construction
  source(file.path("01_Construction.R"))
  
  # 2. Operation
  source(file.path("01_Operation.R"))

# 3) Load results
rm(list = ls())
options(scipen = 14)
source(file.path("code","packages.R"))

  #1. Construction
  source(file.path("02_Construction.R"))

  # 2. Operation
  source(file.path("02_Operation.R"))
