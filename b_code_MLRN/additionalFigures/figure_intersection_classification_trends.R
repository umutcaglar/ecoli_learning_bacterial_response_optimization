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


###**** int mrna Carbon ****####
pick_data_choice = "int_mrna"
testConditions = c("carbonSource")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_carbonSourece_trace
###*****************************


###**** int mrna Growth ****####
pick_data_choice = "int_mrna"
testConditions = c("growthPhase")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_growthPhase_trace
###*****************************


###**** int mrna Mg_mM_Levels ****####
pick_data_choice = "int_mrna"
testConditions = c("Mg_mM_Levels")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_Mg_mM_Levels_trace
###*****************************


###**** int mrna Na_mM_Levels ****####
pick_data_choice = "int_mrna"
testConditions = c("Na_mM_Levels")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_Na_mM_Levels_trace
###*****************************


###**** int protein carbonSource  ****####
pick_data_choice = "int_protein"
testConditions = c("carbonSource")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_protein_carbonSource_trace
###*****************************


###**** int protein growthPhase ****####
pick_data_choice = "int_protein"
testConditions = c("growthPhase")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_protein_growthPhase_trace
###*****************************


###**** int protein Mg_mM_Levels****####
pick_data_choice = "int_protein"
testConditions = c("Mg_mM_Levels")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_protein_Mg_mM_Levels_trace
###*****************************


###**** int protein Na_mM_Levels****####
pick_data_choice = "int_protein"
testConditions = c("Na_mM_Levels")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_protein_Na_mM_Levels_trace
###*****************************


###**** int mrna protein Carbon ****####
pick_data_choice = "int_mrna_protein"
testConditions = c("carbonSource")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_protein_carbonSourece_trace
###*****************************


###**** int mrna protein Growth ****####
pick_data_choice = "int_mrna_protein"
testConditions = c("growthPhase")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_protein_growthPhase_trace
###*****************************


###**** int mrna protein Mg_mM_Levels ****####
pick_data_choice = "int_mrna_protein"
testConditions = c("Mg_mM_Levels")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_protein_Mg_mM_Levels_trace
###*****************************


###**** int mrna protein Na_mM_Levels ****####
pick_data_choice = "int_mrna_protein"
testConditions = c("Na_mM_Levels")
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

read.csv(file = paste0("../b_results_v2/outputDF_", parameterName,".csv"))%>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)%>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction->int_mrna_protein_Na_mM_Levels_trace
###*****************************


###**** DF Generation ****####
complex_int_mrna_growthPhase=data.frame(Percentage = int_mrna_growthPhase_trace, condition="int_mrna_growth", dataType= "mrna", dataSet = "int", test="growth")
complex_int_mrna_carbonSource=data.frame(Percentage = int_mrna_carbonSourece_trace, condition="int_mrna_carbon", dataType= "mrna", dataSet = "int", test="carbon")
complex_int_mrna_MgLevels=data.frame(Percentage = int_mrna_Mg_mM_Levels_trace, condition="int_mrna_MgLevels", dataType= "mrna", dataSet = "int", test="MgLevels")
complex_int_mrna_NaLevels=data.frame(Percentage = int_mrna_Na_mM_Levels_trace, condition="int_mrna_NaLevels", dataType= "mrna", dataSet = "int", test="NaLevels")


complex_int_protein_growthPhase=data.frame(Percentage = int_protein_growthPhase_trace, condition="int_protein_growth", dataType= "protein", dataSet = "int", test="growth")
complex_int_protein_carbonSource=data.frame(Percentage = int_protein_carbonSource_trace, condition="int_protein_carbon", dataType= "protein", dataSet = "int", test="carbon")
complex_int_protein_MgLevels=data.frame(Percentage = int_protein_Mg_mM_Levels_trace, condition="int_protein_MgLevels", dataType= "protein", dataSet = "int", test="MgLevels")
complex_int_protein_NaLevels=data.frame(Percentage = int_protein_Na_mM_Levels_trace, condition="int_protein_NaLevels", dataType= "protein", dataSet = "int", test="NaLevels")


complex_int_mrna_protein_growthPhase=data.frame(Percentage = int_mrna_protein_growthPhase_trace, condition="int_mrna_protein_growth", dataType= "mrna_protein", dataSet = "int", test="growth")
complex_int_mrna_protein_carbonSource=data.frame(Percentage = int_mrna_protein_carbonSourece_trace, condition="int_mrna_protein_carbon", dataType= "mrna_protein", dataSet = "int", test="carbon")
complex_int_mrna_protein_MgLevels=data.frame(Percentage = int_mrna_protein_Mg_mM_Levels_trace, condition="int_mrna_protein_MgLevels", dataType= "mrna_protein", dataSet = "int", test="MgLevels")
complex_int_mrna_protein_NaLevels=data.frame(Percentage = int_mrna_protein_Na_mM_Levels_trace, condition="int_mrna_protein_NaLevels", dataType= "mrna_protein", dataSet = "int", test="NaLevels")


###*****************************
# R bind all DFs
dplyr::bind_rows(complex_int_mrna_growthPhase,
                 complex_int_mrna_carbonSource,
                 complex_int_mrna_MgLevels,
                 complex_int_mrna_NaLevels,
                 complex_int_protein_growthPhase,
                 complex_int_protein_carbonSource,
                 complex_int_protein_MgLevels,
                 complex_int_protein_NaLevels,
                 complex_int_mrna_protein_growthPhase,
                 complex_int_mrna_protein_carbonSource,
                 complex_int_mrna_protein_MgLevels,
                 complex_int_mrna_protein_NaLevels)->combined
###*****************************


###*****************************
# generate summary
combined %>%
  dplyr::group_by(condition, dataType, dataSet, test) %>%
  dplyr::summarise(sumPercentage=sum(Percentage), 
                   success_v1=sum(Percentage)/100,
                   totLength=length(Percentage)) %>%
  dplyr::mutate(success_v2=sumPercentage/(100*totLength)) %>%
  dplyr::mutate(success_v3=1+(sumPercentage-100)/(100* (totLength-1) )) ->combined_summary
###*****************************


###*****************************
# Change order
combined_summary$dataType <- factor(combined_summary$dataType, 
                                levels=c("mrna","protein","mrna_protein"))
###*****************************


###*****************************
# Generate figure
fig01<-ggplot(data = combined_summary, mapping = aes(x=dataType,y=success_v3, color=test, group=test))+
  geom_point(size=2, alpha=.8)+
  geom_line()+
  geom_hline(yintercept = 1, colour="grey40", linetype = 2)+
  geom_hline(yintercept = 2, colour="grey40", linetype = 2)+
  xlab("Complex Tests") + ylab("Success") +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0.9,2.1), 
                     breaks = c(1,2), 
                     labels = c("Random","Perfect"))

print(fig01)
###*****************************


###*****************************
cowplot::save_plot(filename = "intersection_data_success.jpeg", plot = fig01, ncol = 2)
###*****************************