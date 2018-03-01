# Compare the results of old and new on protein data

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************



# OLD WAY


###*****************************
# Parameters
analyzeName="protein"
pick_data_="protein"
growthPhase="ExpAllPhase"
testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
ndivisionCost_=55
ndivisionGamma_=31
numRepeatsFor_TestTrainSubset_Choice_=60
mtrylistRF=paste(seq(1,7),collapse = "_")
doNotSave=0 # save the square table figures. 1 means DO NOT save
costFunction_="F1_final"

testConditionsCombined=paste0(testConditions,collapse = "_")
###*****************************


###*****************************
# read the list to find file name
timeStampFile<-read.csv(file = paste0("../../b_results/","parametersModelFitMetafile",".csv")) #import file
timeStampFile %>%
  dplyr::filter(pick_data==get("pick_data_")) %>%
  dplyr::filter(growthPhase_names==get("growthPhase")) %>%
  dplyr::filter(numRepeatsFor_TestTrainSubset_Choice==get("numRepeatsFor_TestTrainSubset_Choice_")) %>%
  dplyr::filter(ndivisionCost==get("ndivisionCost_")) %>%
  dplyr::filter(ndivisionGamma==get("ndivisionGamma_")) %>%
  dplyr::filter(testConditions==get("testConditionsCombined"))%>%
  dplyr::filter(costFunction==get("costFunction_"))->chosenDataSetInfo


if(nrow(chosenDataSetInfo)!=1){stop("one than one file selected")}

fileName=as.vector(chosenDataSetInfo$fileName)
load(file = paste0("../../b_results/",fileName,".RDA"))
###*****************************

parallel_Result[[1]]$performanceDf_linear-> performance_df
parallel_Result[[1]]$resultListSVM_linear-> results_df
performance_df %>% 
  dplyr::group_by(costList) %>% 
  dplyr::summarize(mean_error_val = mean(error.val))->q
###*****************************


###*****************************
# the new results
load(file = paste0("../pipeline_data/data_list_protein_part2.RDA"))
load(file = paste0("../pipeline_data/f1_protein_part3.RDA"))
###*****************************

