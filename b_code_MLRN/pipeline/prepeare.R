# prepeare

###*****************************
# REQUIRED LIBRARIES
# Data tracking
require("magrittr")
require("tidyverse")

# Graphing
require("cowplot")
require("gtable") # for the "gtable_filter" function to seperately save the legend
require("grid") # For manipulating ggplot obj

# Machine learning
require("ape") # for pcoa (# the "pcoa" function)
require("vegan") # for pcoa (# the "vegdist" function)
require("e1071") # for svm
require("MASS") # to find matrix inverses
require("randomForest") # for the random forest
require("Rcpp")

# Batch Correction
require("sva") # only for machine learning

# Text manipulation
require("stringr")

# Parallel
require("doMC")
require("foreach")
###*****************************

###*****************************
source("pipeline/PCA_PCoA_func.R")
source("pipeline/batchCorrectionSVA.R")
source("pipeline/dataPreperationComb_func.R")
source("pipeline/dataPreperation_func.R")
source("pipeline/f1_fun.R")
###*****************************