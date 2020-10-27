# -----------------------------------------------------------------
# IMPORT LIBRARIES
# -----------------------------------------------------------------
require("tidyverse")
require("MASS")
require("gridExtra")
require("xtable")
# -----------------------------------------------------------------
# READ THE DATA
# -----------------------------------------------------------------
raw_logistic <- read.csv("/Users/laurasansc/github/statistical_modelling/data/Logistic.txt", sep="\t")
raw_trial <- read.csv("/Users/laurasansc/github/statistical_modelling/data/actg320.txt", sep="\t")


# -----------------------------------------------------------------
# PRESENT THE DATA - LOGISTIC DATA
# -----------------------------------------------------------------
# PRINT TABLE
print(xtable(raw_logistic,digits=5))

# -----------------------------------------------------------------
# WRANGLE DATA - ACTG320 DATA
# -----------------------------------------------------------------
raw_trial = raw_trial[c("time", "event", "tx")]