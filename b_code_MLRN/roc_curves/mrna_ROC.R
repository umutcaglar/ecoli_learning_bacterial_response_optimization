# ROC curve genefating script -mRNA


###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response_optimization/b_code_MLRN/"))} # mac computer
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
require("randomForest") # for the random forest
require("ROCR")

# Batch Correction
require("sva") # only for machine learning
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
                   filterGenes = "noMatchFilter", # can be either "noFilter", or any combination of c("meanFilter", "maxFilter", "sdFilter", "noMatchFilter")
                   threshold=NA, # the threshold value for "meanFilter", "maxFilter", "sdFilter" can be  c(meanFilter=5,maxFilter=3,sdFilter=7)
                   roundData=TRUE,
                   sumTechnicalReplicates=TRUE,
                   deSeqSfChoice="p1Sf", # can be "regSf", "p1Sf"
                   normalizationMethodChoice= "vst", # can be "vst", "rlog", "log10", "noNorm"
                   test_for = "noTest")  # works only if normalizationMethodChoice == noNorm
# c("Mg_mM_Levels", "Na_mM_Levels", "growthPhase", "carbonSource", "noTest")
dataNameDF=as.data.frame(dataName[1])

metaDataName=dataNameDF
metaDataName$objectName.initial="metaData"

dataName=paste(dataNameDF,collapse = "_")
metaDataName=paste(metaDataName,collapse = "_")

mainDataFrame=read.csv(file = paste0("../a_results/",dataName,".csv"),header = TRUE,row.names = 1)
condition=read.csv(file = paste0("../a_results/",metaDataName,".csv"),header = TRUE)
###*****************************


###*****************************
# Trial Reletad Parameters
#dimensionChoice=11
numRepeatsFor_TestTrainSubset_Choice=200 #60 #how many times will I divide the data as train&tune vs test
percentTest=.20 #Should be a number between 0-1
# sum of percentTest and percentTune shoul not be smaller than 1
testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase") # different combinations that we will look into
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
dimReductionType="PCA" # Can be PCA, PCoA, noReduction
dimensionChoiceValue=10 # does not work with dimReductionType="noReduction"

batchCorrectionMethod<-"fSVA"
classWeightInputType="on SVA" # for probabilistic it should be after SVA
similarDataClassifierForBatch=c("Na_mM","Mg_mM","carbonSource_Short","growthPhase")

# SVM parameters
type_svmChoice="C-classification" #Can be "C-classification" but not "eps-regression"
#kernel_typeChoice="radial"

# SVM parameter
Cost_linear=68.12921
Cost_radial=1000
Gamma_radial=0.03162278
Cost_sigmoidal=1000
Gamma_sigmoidal=0.02154435
kernelList=c("linear","radial","sigmoid") # kernel vector

# RF parameters
ntree_RF=10000
nodesize_RF=2
mtry_RF=2

# Unnecessary Parameter
crossValue=1

# combined set related variables
batchCorrectionType="separate" # can be together or separate for joined datasets
if(!(grepl(pattern = "mrna",x = dataNameDF$objectName.pick_data) &
     grepl(pattern = "protein",x = dataNameDF$objectName.pick_data)))
{batchCorrectionType = "together"}

# arrange tuning
tuning = TRUE

# parallel processing
parallel_com = TRUE

# CostFunction
costFunction="F1_corrected"
###*****************************


###*****************************
# Remaining parts of the code
if(dataNameDF$objectName.pick_data!="int_mrna_protein")
{source("roc_curves/ROC_mainCode.R")}

if(dataNameDF$objectName.pick_data=="int_mrna_protein")
{source("roc_curves/ROC_mainCodeComb.R")}
###*****************************

