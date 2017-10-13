# ANALYSIS OF OUT OF HOUSE DATA

# Schmidt, Alexander, et al. "The quantitative and condition-dependent 
#Escherichia coli proteome." Nature biotechnology 34.1 (2016): 104.


###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
###*****************************


###*****************************
# REQUIRED LIBRARIES
require(tidyverse)
require(stringr)
require(cowplot)
###*****************************


###*****************************
# Read Data
dataSet1_notClean = read.csv(file = "SuppFor_Paper2/tableS4_PCount.csv")
dataSet2_notClean = read.csv(file = "SuppFor_Paper2/tableS5_PCount.csv")
dataSet_comb_notClean = read.csv(file = "SuppFor_Paper2/tableS6_PCount.csv")

proteinData = read.csv("resDf_protein_trT_set00_StcYtcNasAgrNgrMgh_SYAN_baseMgAllMg_baseNaAllNa_ExpAllPhase_noMatchFilter_p1Sf_vst.csv")
proteinMetaData = read.csv("metaData_protein_trT_set00_StcYtcNasAgrNgrMgh_SYAN_baseMgAllMg_baseNaAllNa_ExpAllPhase_noMatchFilter_p1Sf_vst.csv")

dictionary = read.csv("nameDictionary_RNA&Protein.csv")
###*****************************


###*****************************
# Check if combined is coming from 1 and 2
dataSet_comb_notClean %>%
  dplyr::filter(Dataset == 1) %>%
  dplyr::select(Gene, Glucose) %>%
  dplyr::arrange(Gene) %>%
  head(.)

dataSet1_notClean %>%
  dplyr::select(Gene, glucose) %>%
  dplyr::arrange(Gene) %>%
  head(.)

dataSet_comb_notClean %>%
  dplyr::filter(Dataset == 2) %>%
  dplyr::select(Gene, Glucose) %>%
  dplyr::arrange(Gene) %>%
  head(.)

dataSet2_notClean %>%
  dplyr::select(Gene, Glucose) %>%
  dplyr::arrange(Gene) %>%
  head(.)

# Data set 2 has same data with with the combined data set but data set 1 does not.
###*****************************


# Keep Going with data set 2
###*****************************
colnames(dataSet2_notClean)
dataSet2_notClean %>%
  dplyr::select(gene_name = Gene, 
                glucose = Glucose,
                glycerol = Glycerol,
                Na_50_stress = Osmotic.stress.glucose,
                stationary = Stationary.phase.1.day,
                late_stationary = Stationary.phase.3.days) -> dataSet2
###*****************************


###*****************************
# Pick the training data
load("../b_results/RunNo_2017_04_20_05_45_59_907834.RDA")
###*****************************

###*****************************
# Train the whole data with desired winning model
modelSVM_sigmoid<-e1071::svm(data = trainDataFrame,
                             conditionInvestigated~.,
                             type = "C-classification",
                             kernel = "sigmoid",
                             class.weights = classWeightVector,
                             cost=2154.435,
                             gamma=0.04641)
###*****************************



###*****************************