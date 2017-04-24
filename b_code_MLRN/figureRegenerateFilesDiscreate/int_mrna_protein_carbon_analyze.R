# This part will generate figures and organized tables for multiple parallel runs



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
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
require("gtable") # for the "gtable_filter" function to seperately save the legend
require("grid") # For manipulating ggplot obj

# Text manipulation
require("stringr")

# the F1 function requirement
require("Rcpp")
###*****************************


###*****************************
#Load Functions
source("../a_code_dataPreperation_RNA&Protein/replace_fun.R")
sourceCpp("pipeline/f1ScoreFunction.cpp")
###*****************************


###*****************************
# Parameters
analyzeName="int_mRNA_protein_carbon"
pick_data="int_mrna_protein"
growthPhase="ExpAllPhase"
testConditions=c("carbonSource")
ndivisionCost=55
ndivisionGamma=31
numRepeatsFor_TestTrainSubset_Choice=60
doNotSave=0 # save the square table figures. 1 means DO NOT save
costFunction="F1_final"

testConditionsCombined=paste0(testConditions,collapse = "_")
###*****************************


###*****************************
# read the list to find file name
timeStampFile<-read.csv(file = paste0("../b_results/","parametersModelFitMetafile",".csv")) #import file
timeStampFile %>%
  dplyr::filter(pick_data==get("pick_data")) %>%
  dplyr::filter(growthPhase_names==get("growthPhase")) %>%
  dplyr::filter(numRepeatsFor_TestTrainSubset_Choice==get("numRepeatsFor_TestTrainSubset_Choice")) %>%
  dplyr::filter(ndivisionCost==get("ndivisionCost")) %>%
  dplyr::filter(ndivisionGamma==get("ndivisionGamma")) %>%
  dplyr::filter(testConditions==get("testConditionsCombined"))%>%
  dplyr::filter(costFunction==get("costFunction"))->chosenDataSetInfo

chosenDataSetInfo <- chosenDataSetInfo[2,]


if(nrow(chosenDataSetInfo)!=1){stop("more than one file selected")}

fileName=as.vector(chosenDataSetInfo$fileName)
load(file = paste0("../b_results/",fileName,".RDA"))
###*****************************



###*****************************
# call the main analyze file
source("figureRegenerateFilesDiscreate/mainAnalyzeCode.R")
###*****************************
