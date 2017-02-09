# The machine learning script

# The steps for the script
# This part will be repeated many times with for loop
#     It take the data (mRNA or Protein), divide the data into 2 parts (intelligently) as training & control
#     This part will be repeated many times with for loop
#       In the training data, some of the data is trashed (randomly) in order to make the number of elements in 
#       each group comparible
#       remaining data will be used for training
#       after PCA or PCoA (If they have a difference)
#       we will pick top columns for analyse (sqrt n columns)
#       depending on the data type we will use discreate SVM or continious SVM (Both are in radial basis)
#       Than we will use the control set to see 


### ****************************
# In this code I will do PCA or PCoA before seperating data. 
# This improves the results. In future it will be replaced with proper rotation of test matrix
### ****************************


###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
set.seed(14159)
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
require("magrittr")
require("DESeq2")
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
require("gtable") # for the "gtable_filter" function to seperately save the legend
require("grid") # For manipulating ggplot obj

# Machine learning
require("ape") # for pcoa (# the "pcoa" function)
require("vegan") # for pcoa (# the "vegdist" function)
require("e1071") # for svm
require("MASS") # to find matrix inverses

# Batch Correction
require("sva") # only for machine learning

# Text manipulation
require("stringr")
###*****************************


###*****************************
#Load Functions
source("pipeline/dataChoiceFunctionSingle.R")
source("pipeline/PCA_PCoA_func.R")
source("pipeline/batchCorrectionSVA.R")
source("pipeline/dataPreperationComb_func.R")
source("pipeline/dataPreperation_func.R")
source("../a_code_dataPreperation_RNA&Protein/data_naming_functions.R")
source("../a_code_dataPreperation_RNA&Protein/replace_fun.R")	
###*****************************


###*****************************
# Find the csv files that need to be imported
dataName=name_data(initialValue=c("resDf"), # can be c("genes0.05","genes_P0.05Fold2","resDf")
                   dataType = "mrna", 
                   # can be "rna", "mrna", "protein", "protein_wo_NA", 
                   #        "int_mrna_protein", "int_mrna", "int_protein" 
                   badDataSet = "set00", # can be "set00",set01","set02", "set03"
                   # referenceParameters can be a vector like
                   # c("growthPhase", "Mg_mM_Levels", "Na_mM_Levels", "carbonSource", "experiment")
                   referenceParameters=c("growthPhase",
                                         "Mg_mM_Levels", 
                                         "Na_mM_Levels", 
                                         "carbonSource",
                                         "experiment"),
                   # referenceLevels can be a vector like
                   # c("exponential", "baseMg", "baseNa", "glucose", "glucose_time_course")
                   referenceLevels=c("exponential",
                                     "baseMg", 
                                     "baseNa", 
                                     "glucose", 
                                     "glucose_time_course"),
                   # Can be "glucose_time_course", "MgSO4_stress_low", "MgSO4_stress_high"
                   experimentVector = c("allEx"), # can be "Stc","Ytc","Nas","Agr","Ngr","Mgl","Mgh" // "allEx"
                   carbonSourceVector = "SYAN", # can be any sub combination of "SYAN"
                   MgLevelVector = c("allMg"), # can be "lowMg","baseMg","highMg" // "allMg"
                   NaLevelVector = c("allNa"), # can be "baseNa","highNa" // "allNa"
                   growthPhaseVector = c("allPhase"), # can be "exponential","stationary","late_stationary" // "allPhase"
                   filterGenes = "noFilter", # can be "noFilter", "meanFilter", "maxFilter", "sdFilter" 
                   threshold=NA, # the threshold value for "meanFilter", "maxFilter", "sdFilter"
                   roundData=TRUE,
                   sumTechnicalReplicates=TRUE,
                   deSeqSfChoice="p1Sf", # can be "regSf", "p1Sf"
                   normalizationMethodChoice= "vst", # can be "vst", "rlog", "log10", "noNorm"
                   test_for = "noTest")  # works only if normalizationMethodChoice == noNorm
# c("Mg_mM_Levels", "Na_mM_Levels", "growthPhase", "carbonSource", "noTest")
dataNameDF=as.data.frame(dataName[1])
colnames(dataNameDF) <- gsub(pattern = "objectName.", replacement = "", x = colnames(dataNameDF))

metaDataName=dataNameDF
metaDataName$initial="metaData"
treeDataName=dataNameDF
treeDataName$initial="treeData"
heatMapName=dataNameDF
heatMapName$initial="heatMap"

dataName=paste(dataNameDF,collapse = "_")
metaDataName=paste(metaDataName,collapse = "_")
treeDataName=paste(treeDataName,collapse = "_")
heatMapName=paste(heatMapName,collapse = "_")

mainDataFrame=read.csv(file = paste0("../a_results/",dataName,".csv"),header = TRUE,row.names = 1)
condition=read.csv(file = paste0("../a_results/",metaDataName,".csv"),header = TRUE)
###*****************************


###*****************************
# Trial Reletad Parameters
#dimensionChoice=11
numRepeatsFor_TestTrainSubset_Choice=1000 #needs division for more efficient algorithm and memory stuff
percentTest=.20 #Should be a number between 0-1
testConditions=c("Na_mM_Levels") # different combinations that we will look into 
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
dimReductionType="PCA" # Can be PCA or PCoA
type_svmChoice="C-classification" #Can be "C-classification" but not "eps-regression" 
kernel_typeChoice="radial"

batchCorrectionMethod<-"fSVA"
classWeightInputType="on SVA" # for probabilistic it should be after SVA
similarDataClassifierForBatch=c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")


# combined set related variables
batchCorrectionType="separate" # can be together or separate for joined datasets
if(!(grepl(pattern = "mrna",x = dataNameDF$pick_data) & 
     grepl(pattern = "protein",x = dataNameDF$pick_data)))
{batchCorrectionType = "together"}

# arrange tuning
tuning = FALSE
if(type_svmChoice == "C-classification"){tuning==FALSE}

# parallel processing
parallel_com = FALSE
numCore = 1
if(type_svmChoice == "C-classification"){parallel_com = FALSE}
if(parallel_com == FALSE){numCore = 1}
###*****************************


#Initial DataFrame preperation
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


###*****************************
# intermediate variables
numRepeatsFor_TestTrainSubset=numRepeatsFor_TestTrainSubset_Choice
batchCorrectionType_Choice = batchCorrectionType
dimensionChoice = round(sqrt(round(nrow(condition)*.8)))
similarDataClassifierForBatch_Choice = paste(similarDataClassifierForBatch, collapse = "_")
###*****************************


###*****************************
# load the meta file related with runs
metaRunsFile<-read.csv(file = "../b_results_v2/parametersCombined.csv")
###*****************************


###*****************************
# load the result of the loop
metaRunsFile %>%
  dplyr::filter(pick_data == dataNameDF$pick_data) %>%
  dplyr::filter(testFor == paste(testConditions, collapse = "_"))%>%
  dplyr::filter(growthPhase_names == dataNameDF$growthPhase_names)%>%
  dplyr::filter(type_svm == type_svmChoice) %>%
  dplyr::filter(numRepeatsFor_TestTrainSubset_Choice == numRepeatsFor_TestTrainSubset) %>%
  dplyr::filter(batchCorrectionType == batchCorrectionType_Choice)%>%
  dplyr::filter(dimensions == paste0("D", dimensionChoice) )%>%
  dplyr::filter(similarDataClassifierForBatch == similarDataClassifierForBatch_Choice)-> parameterVector
###*****************************


###*****************************
# Decide Conditions
modelSVM <-c()
parameterVector %>% .$initial %>% as.vector(.)->fileName
parameterVector %>% .$cost -> modelSVM$cost
parameterVector %>% .$gamma -> modelSVM$gamma
###*****************************


###*****************************
# Load Results
result_List=read.csv(file=paste0("../b_results_v2/outputDF_",fileName,".csv"))
###*****************************


###*****************************
result_List %>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)->result_ListSum
###*****************************


# Figures and save
###*****************************####
doNotSave=TRUE
source("pipeline/machineLearning_subCode_figure_save.R")
###*****************************