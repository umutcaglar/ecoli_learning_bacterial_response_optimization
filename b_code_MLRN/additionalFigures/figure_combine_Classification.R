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





###**** mrna Carbon all Phase + carbon color legend ****####
pick_data_choice = "mrna"
testConditions = c("carbonSource")
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
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_carbonSource_allPhase<-figComb
carbonColorLegend <- ggplotGrob(color_legend)
###*****************************

###**** mrna Growth allPhase +  Growth Color Legend ****####
pick_data_choice = "mrna"
testConditions = c("growthPhase")
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
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_growthPhase_allPhase<-figComb
growthColorLegend <- ggplotGrob(color_legend)
###*****************************

###**** mrna Mg allPhase +  Mg Color Legend ****####
pick_data_choice = "mrna"
testConditions = c("Mg_mM_Levels")
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
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_Mg_mM_Levels_allPhase<-figComb
MgColorLegend <- ggplotGrob(color_legend)
###*****************************

###**** mrna Na allPhase +   Na Color Legend ****####
pick_data_choice = "mrna"
testConditions = c("Na_mM_Levels")
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
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_Na_mM_Levels_allPhase<-figComb
NaColorLegend <- ggplotGrob(color_legend)
###*****************************



###**** protein carbonSource allPhase ****####
pick_data_choice = "protein"
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
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_carbonSource_allPhase<-figComb
###*****************************

###**** protein growthPhase allPhase ****####
pick_data_choice = "protein"
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
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_growthPhase_allPhase<-figComb
###*****************************

###**** protein MgLevels allPhase ****####
pick_data_choice = "protein"
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
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_Mg_mM_Levels_allPhase<-figComb
###*****************************

###**** protein NaLevels allPhase ****####
pick_data_choice = "protein"
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
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_Na_mM_Levels_allPhase<-figComb
###*****************************



###**** mrna Carbon Exp-Phase ****####
pick_data_choice = "mrna"
testConditions = c("carbonSource")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Exp" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 8
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_carbonSource_Exp<-figComb
###*****************************

###**** mrna MgLevels Exp-Phase ****####
pick_data_choice = "mrna"
testConditions = c("Mg_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Exp" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 8
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_Mg_mM_Levels_Exp<-figComb
###*****************************

###**** mrna NaLevels Exp-Phase ****####
pick_data_choice = "mrna"
testConditions = c("Na_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Exp" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 8
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_Na_mM_Levels_Exp<-figComb
###*****************************



###**** mrna Carbon Sta-Phase ****####
pick_data_choice = "mrna"
testConditions = c("carbonSource")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Sta" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 7
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_carbonSource_Sta<-figComb
###*****************************

###**** mrna MgLevels Sta-Phase ****####
pick_data_choice = "mrna"
testConditions = c("Mg_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Sta" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 7
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_Mg_mM_Levels_Sta<-figComb
###*****************************

###**** mrna NaLevels Sta-Phase ****####
pick_data_choice = "mrna"
testConditions = c("Na_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Sta" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 7
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
mrna_Na_mM_Levels_Sta<-figComb
###*****************************



###**** protein Carbon Exp-Phase ****####
pick_data_choice = "protein"
testConditions = c("carbonSource")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Exp" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 7
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_carbonSource_Exp<-figComb
###*****************************

###**** protein MgLevels Exp-Phase ****####
pick_data_choice = "protein"
testConditions = c("Mg_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Exp" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 7
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_Mg_mM_Levels_Exp<-figComb
###*****************************

###**** protein NaLevels Exp-Phase ****####
pick_data_choice = "protein"
testConditions = c("Na_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Exp" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 7
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_Na_mM_Levels_Exp<-figComb
###*****************************



###**** protein Carbon Sta-Phase ****####
pick_data_choice = "protein"
testConditions = c("carbonSource")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Sta" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 5
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_carbonSource_Sta<-figComb
###*****************************

###**** protein MgLevels Sta-Phase ****####
pick_data_choice = "protein"
testConditions = c("Mg_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Sta" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 5
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_Mg_mM_Levels_Sta<-figComb
###*****************************

###**** protein NaLevels Sta-Phase ****####
pick_data_choice = "protein"
testConditions = c("Na_mM_Levels")
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
growthPhase_names_choice = "Sta" 
# Options ExpAllPhase, Exp, Sta
type_svm_choice = "C-classification" 
# Options "C-classification", "eps-regression" 
numRepeatsFor_TestTrainSubset = 1000
batchCorrectionType_Choice = "together"
dimensionChoice = 5
similarDataClassifierForBatch_Choice = c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


metaRunsFile %>% filteringObj %>% .$initial %>% as.vector(.) -> parameterName
if(length(parameterName)!=1){stop()}
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
protein_Na_mM_Levels_Sta<-figComb
###*****************************



###**** mrna protein all Conditions all Phase + full color legend ****####
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
load(paste0("../b_results_v2/fig_obj_", parameterName,".Rda"))
combined_AllData<-figComb
fullColorLegend <- ggplotGrob(color_legend)
###*****************************



###**** Generate Cowplot Figures ****####
#Fig1 mRNA (all data)
fig_mrna_allPhase<-cowplot::plot_grid(mrna_growthPhase_allPhase,mrna_carbonSource_allPhase,
                                      mrna_Mg_mM_Levels_allPhase,mrna_Na_mM_Levels_allPhase,
                                      labels = c("A", "B","C","D"),align="v",scale=.9)

# Add percent legend
g.main <- ggplotGrob(fig_mrna_allPhase)
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(2, "in")), index$l+2)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+2,
                          b = index$b,
                          r = index$r+2,
                          name="label")

# Add the color legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.05, "null")), index$t+2)
g.main <- gtable_add_grob(g.main, fullColorLegend,
                          t = index$b+2,
                          l = index$l,
                          b = index$b+2,
                          r = index$r,
                          name="fullColorLegend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "fullColorLegend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)

# print figure
fig_mrna_allPhase=ggdraw(g.main)
print(fig_mrna_allPhase)
###*****************************


###*****************************
#Fig2 protein (all data)
fig_protein_allPhase<-cowplot::plot_grid(protein_growthPhase_allPhase,protein_carbonSource_allPhase,
                                         protein_Mg_mM_Levels_allPhase,protein_Na_mM_Levels_allPhase,
                                         labels = c("A", "B","C","D"),align="v",scale=.9)

# Add percent legend
g.main <- ggplotGrob(fig_protein_allPhase)
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(2, "in")), index$l+2)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+2,
                          b = index$b,
                          r = index$r+2,
                          name="label")

# Add the color legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.05, "null")), index$t+2)
g.main <- gtable_add_grob(g.main, fullColorLegend,
                          t = index$b+2,
                          l = index$l,
                          b = index$b+2,
                          r = index$r,
                          name="fullColorLegend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "fullColorLegend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)

# print Figure
fig_protein_allPhase=ggdraw(g.main)
print(fig_protein_allPhase)
###*****************************


###*****************************
#Fig3 carbon (trend)
fig_carbon_trend<-cowplot::plot_grid(mrna_carbonSource_allPhase, mrna_carbonSource_Exp, mrna_carbonSource_Sta,
                                     protein_carbonSource_allPhase, protein_carbonSource_Exp,protein_carbonSource_Sta,
                                     labels = c("A", "B", "C", "D", "E", "F"),align="v",nrow = 2,scale=.9)

# Add percent legend
g.main <- ggplotGrob(fig_carbon_trend)
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(2, "in")), index$l+2)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+2,
                          b = index$b,
                          r = index$r+2,
                          name="label")

# Add the color legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.05, "null")), index$t+2)
g.main <- gtable_add_grob(g.main, carbonColorLegend,
                          t = index$b+2,
                          l = index$l,
                          b = index$b+2,
                          r = index$r,
                          name="carbonColorLegend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "carbonColorLegend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)

# print Figure
fig_carbon_trend=ggdraw(g.main)
print(fig_carbon_trend)
###*****************************
# 
# 
###*****************************
#Fig4 mg (trend)
fig_mg_trend<-cowplot::plot_grid(mrna_Mg_mM_Levels_allPhase, mrna_Mg_mM_Levels_Exp, mrna_Mg_mM_Levels_Sta,
                                 protein_Mg_mM_Levels_allPhase, protein_Mg_mM_Levels_Exp, protein_Mg_mM_Levels_Sta,
                                 labels = c("A", "B", "C", "D", "E", "F"),align="h",nrow = 2,scale=.9)

# Add percent legend
g.main <- ggplotGrob(fig_mg_trend)
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(2, "in")), index$l+2)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+2,
                          b = index$b,
                          r = index$r+2,
                          name="label")

# Add the color legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.05, "null")), index$t+2)
g.main <- gtable_add_grob(g.main, MgColorLegend,
                          t = index$b+2,
                          l = index$l,
                          b = index$b+2,
                          r = index$r,
                          name="MgColorLegend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "MgColorLegend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)

# print Figure
fig_mg_trend=ggdraw(g.main)
print(fig_mg_trend)
###*****************************


###*****************************
#Fig5 na (trend)
fig_na_trend<-cowplot::plot_grid(mrna_Na_mM_Levels_allPhase, mrna_Na_mM_Levels_Exp, mrna_Na_mM_Levels_Sta,
                                 protein_Na_mM_Levels_allPhase, protein_Na_mM_Levels_Exp, protein_Na_mM_Levels_Sta,
                                 labels = c("A", "B", "C", "D", "E", "F"),align="h",nrow = 2,scale=.9)

# Add percent legend
g.main <- ggplotGrob(fig_na_trend)
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(2, "in")), index$l+2)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+2,
                          b = index$b,
                          r = index$r+2,
                          name="label")

# Add the color legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.05, "null")), index$t+2)
g.main <- gtable_add_grob(g.main, NaColorLegend,
                          t = index$b+2,
                          l = index$l,
                          b = index$b+2,
                          r = index$r,
                          name="NaColorLegend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "NaColorLegend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)


# print Figure
fig_na_trend=ggdraw(g.main)
print(fig_na_trend)
###*****************************


###**** Save Figures ****####
save_plot("../b_figures_v2/combinedClassificationFigures/mrna_allPhase.pdf", fig_mrna_allPhase,
          ncol = 2.7,nrow = 2.2,base_aspect_ratio = 1.3)
save_plot("../b_figures_v2/combinedClassificationFigures/protein_allPhase.pdf", fig_protein_allPhase,
          ncol = 2.7,nrow = 2.2,base_aspect_ratio = 1.3)

save_plot("../b_figures_v2/combinedClassificationFigures/carbon_trend.pdf", fig_carbon_trend,
          ncol = 3.7,nrow = 2.2,base_aspect_ratio = 1.3)
save_plot("../b_figures_v2/combinedClassificationFigures/mg_trend.pdf", fig_mg_trend,
          ncol = 3.7,nrow = 2.2,base_aspect_ratio = 1.3)
save_plot("../b_figures_v2/combinedClassificationFigures/na_trend.pdf", fig_na_trend,
          ncol = 3.7,nrow = 2.2,base_aspect_ratio = 1.3)


###*****************************



