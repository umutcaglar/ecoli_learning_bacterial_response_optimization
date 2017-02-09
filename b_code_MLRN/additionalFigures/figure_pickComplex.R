# Combining Figures of Regression

# This code will bring together the figure objects associated with regression analysis

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
###*****************************

###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response/b_code_MLRN_v2/"))} # mac computer
###*****************************


###*****************************
# REQUIRED LIBRARIES
# Data tracking
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
require("grid")
require("gtable")
###*****************************

# 
# ###*****************************
# # Load Files

###*****************************
# load the meta file related with runs
metaRunsFile<-read.csv(file = "../b_results_v2/parametersCombined.csv")
###*****************************


###*****************************
filteringObj<-. %>%
  dplyr::filter(pick_data == pick_data_choice) %>%
  dplyr::filter(testFor == paste(testConditions, collapse = "_"))%>%
  dplyr::filter(growthPhase_names == growthPhase_names_choice)%>%
  dplyr::filter(type_svm == type_svm_choice) %>%
  dplyr::filter(numRepeatsFor_TestTrainSubset_Choice == numRepeatsFor_TestTrainSubset) %>%
  dplyr::filter(batchCorrectionType == batchCorrectionType_Choice)%>%
  dplyr::filter(dimensions == paste0("D", dimensionChoice) )%>%
  dplyr::filter(similarDataClassifierForBatch == paste(similarDataClassifierForBatch_Choice, collapse = "_"))
###*****************************




###**** complex mrna allPhase ****####
pick_data_choice = "mrna"
# can be "mrna", "protein", "int_mrna_protein", "int_mrna", "int_protein" 
testConditions = c("Na_mM_Levels", "Mg_mM_Levels", "carbonSource", "growthPhase")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "ExpAllPhase" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 11
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
file.copy(from = paste0("../b_figures_v2/fig_withLegend_",parameterName,".pdf"),
          to = paste0("../b_figures_v2/complexFigures/complex_", pick_data_choice,".pdf"), overwrite= TRUE)
###*****************************



###**** complex protein allPhase ****####
pick_data_choice = "protein"
# can be "mrna", "protein", "int_mrna_protein", "int_mrna", "int_protein" 
testConditions = c("Na_mM_Levels", "Mg_mM_Levels", "carbonSource", "growthPhase")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "ExpAllPhase" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 9
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
file.copy(from = paste0("../b_figures_v2/fig_withLegend_",parameterName,".pdf"),
          to = paste0("../b_figures_v2/complexFigures/complex_", pick_data_choice,".pdf"), overwrite= TRUE)
###*****************************


###**** complex int_mrna allPhase ****####
pick_data_choice = "int_mrna"
# can be "mrna", "protein", "int_mrna_protein", "int_mrna", "int_protein" 
testConditions = c("Na_mM_Levels", "Mg_mM_Levels", "carbonSource", "growthPhase")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "ExpAllPhase" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 9
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
file.copy(from = paste0("../b_figures_v2/fig_withLegend_",parameterName,".pdf"),
          to = paste0("../b_figures_v2/complexFigures/complex_", pick_data_choice,".pdf"), overwrite= TRUE)
###*****************************


###**** complex int_mrna allPhase ****####
pick_data_choice = "int_protein"
# can be "mrna", "protein", "int_mrna_protein", "int_mrna", "int_protein" 
testConditions = c("Na_mM_Levels", "Mg_mM_Levels", "carbonSource", "growthPhase")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "ExpAllPhase" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 9
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
file.copy(from = paste0("../b_figures_v2/fig_withLegend_",parameterName,".pdf"),
          to = paste0("../b_figures_v2/complexFigures/complex_", pick_data_choice,".pdf"), overwrite= TRUE)
###*****************************


###**** complex int_mrna allPhase ****####
pick_data_choice = "int_mrna_protein"
# can be "mrna", "protein", "int_mrna_protein", "int_mrna", "int_protein" 
testConditions = c("Na_mM_Levels", "Mg_mM_Levels", "carbonSource", "growthPhase")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "ExpAllPhase" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "separate"
dimensionChoice = 9
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
file.copy(from = paste0("../b_figures_v2/fig_withLegend_",parameterName,".pdf"),
          to = paste0("../b_figures_v2/complexFigures/complex_", pick_data_choice,".pdf"), overwrite= TRUE)
###*****************************
