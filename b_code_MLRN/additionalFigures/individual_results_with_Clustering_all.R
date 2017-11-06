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

F1ScoreErr<-function(y,prediction)
{
  beta=1
  y=make.names(y)
  prediction=make.names(prediction)
  a=as.vector(y); b=as.vector(prediction)
  
  inputs=sort(unique(c(a,b)))
  
  TP=c();
  FP=c();
  FN=c();
  F1=c();
  
  for (counter04 in 1:length(inputs))
  {
    testFor=inputs[counter04]
    
    sum(a==testFor & b==testFor)->TP[counter04]
    sum(a!=testFor & b==testFor)->FP[counter04]
    sum(a==testFor & b!=testFor)->FN[counter04]
    
    F1[counter04]=(2*TP[counter04])/(2*TP[counter04]+FP[counter04]+FN[counter04])
    if(0==2*TP[counter04]+FP[counter04]+FN[counter04]){F1[counter04]==0}
  }
  
  # print(paste0("TP: ", paste0(TP, collapse = " ")));
  # print(paste0("FP: ", paste0(FP, collapse = " ")));
  # print(paste0("FN: ", paste0(FN, collapse = " ")));
  # print(paste0("F1: ", paste0(F1, collapse = " ")));
  
  # Controls
  sum_TP = sum(TP, na.rm =T)
  sum_FP = sum(FP, na.rm =T)
  sum_FN = sum(FN, na.rm =T)
  if(sum_FP!=sum_FN){browser()}
  if(length(y)!=length(prediction)){browser()}
  if(length(y)!=sum_TP + sum_FP){browser()}
  
  F1_err=1-mean(F1, na.rm =T)
  # print(paste0("F1_err: ", F1_err));
  
  # browser()
  return(F1_err)
}
###*****************************


analyzeNameList=c("mRNA", "protein")
pick_dataList=c("mrna", "protein")

for(counter02 in 1: length(analyzeNameList))
{
  ###*****************************
  # Parameters
  analyzeName=analyzeNameList[counter02]
  picked_data=pick_dataList[counter02]
  growthPhase="ExpAllPhase"
  testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
  ndivisionCost_=55
  ndivisionGamma_=31
  numRepeatsFor_TestTrainSubset_Choice=60
  doNotSave=0 # save the square table figures. 1 means DO NOT save
  costFunction_="F1_final"
  
  testConditionsCombined=paste0(testConditions,collapse = "_")
  ###*****************************
  
  
  ###*****************************
  # read the list to find file name
  timeStampFile<-read.csv(file = paste0("../b_results/","parametersModelFitMetafile",".csv")) #import file
  timeStampFile %>%
    dplyr::filter(pick_data==get("picked_data")) %>%
    dplyr::filter(growthPhase_names==get("growthPhase")) %>%
    dplyr::filter(numRepeatsFor_TestTrainSubset_Choice==get("numRepeatsFor_TestTrainSubset_Choice")) %>%
    dplyr::filter(ndivisionCost==get("ndivisionCost_")) %>%
    dplyr::filter(ndivisionGamma==get("ndivisionGamma_")) %>%
    dplyr::filter(testConditions==get("testConditionsCombined"))%>%
    dplyr::filter(costFunction==get("costFunction_"))->chosenDataSetInfo
  

  if(nrow(chosenDataSetInfo)!=1){stop("more than one file selected")}
  
  fileName=as.vector(chosenDataSetInfo$fileName)
  
  load(file = paste0("../b_results/",fileName,".RDA"))
  for (counter01 in 1:timeStampVector$numRepeatsFor_TestTrainSubset_Choice)
  {
    if(counter01==1)
    {
      result_List_linear=parallel_Result[[counter01]]$resultListSVM_linear
      result_List_radial=parallel_Result[[counter01]]$resultListSVM_radial
      result_List_sigmoid=parallel_Result[[counter01]]$resultListSVM_sigmoid
      result_List_RF=parallel_Result[[counter01]]$resultListRF
    }
    
    if(counter01!=1)
    {
      result_List_linear=dplyr::bind_rows(result_List_linear, parallel_Result[[counter01]]$resultListSVM_linear)
      result_List_radial=dplyr::bind_rows(result_List_radial, parallel_Result[[counter01]]$resultListSVM_radial)
      result_List_sigmoid=dplyr::bind_rows(result_List_sigmoid, parallel_Result[[counter01]]$resultListSVM_sigmoid)
      result_List_RF=dplyr::bind_rows(result_List_RF,parallel_Result[[counter01]]$resultListRF)
    }
  }
  
  # Result list DF (best of 60 runs including predictions)
  result_List_linear$model<-"linear"
  result_List_radial$model<-"radial"
  result_List_sigmoid$model<-"sigmoid"
  result_List_RF$model<-"RF"
  
  result_List<-dplyr::bind_rows(result_List_linear,
                                result_List_radial,
                                result_List_sigmoid,
                                result_List_RF)
  result_List$kernel<-NULL
  
  colnames(result_List)[which(colnames(result_List)=="ntreeValue")] <- "ntree"
  colnames(result_List)[which(colnames(result_List)=="mtryValue")] <- "mtry"
  colnames(result_List)[which(colnames(result_List)=="nodesizeValue")] <- "nodesize"
  result_List$analyzeName=analyzeName
  
  if(counter02==1){result_List_comb <- result_List}
  if(counter02!=1){result_List_comb <- dplyr::bind_rows(result_List_comb,result_List)}
}
###*****************************


###*****************************
# add individual predictions
result_List_comb %>%
  tidyr::separate(col = predictedValue, 
                  into = c("Na_mM_Levels_Pred", 
                           "Mg_mM_Levels_Pred", 
                           "carbonSource_Pred", 
                           "growthPhase_Pred"), 
                  sep = "_", remove = FALSE)->result_List_comb
###*****************************


###*****************************
# Calculate F1 scores
result_List_comb %>%
  dplyr::group_by(analyzeName, TestTrainSubsetNo, model) %>%
  dplyr::summarize(error_test=F1ScoreErr(predictedValue, conditionInvestigated),
                   error_test_Na=F1ScoreErr(Na_mM_Levels_Pred, Na_mM_Levels),
                   error_test_Mg=F1ScoreErr(Mg_mM_Levels_Pred, Mg_mM_Levels),
                   error_test_carbon=F1ScoreErr(carbonSource_Pred, carbonSource),
                   error_test_growth=F1ScoreErr(growthPhase_Pred, growthPhase))%>%
  dplyr::mutate(performance_test=1-error_test,
                performance_test_Na=1-error_test_Na,
                performance_test_Mg=1-error_test_Mg,
                performance_test_carbon=1-error_test_carbon,
                performance_test_growth=1-error_test_growth)%>%
  dplyr::group_by(analyzeName, model)%>%
  dplyr::mutate(meanPerformance_test=mean(performance_test),
                meanPerformance_test_Na=mean(performance_test_Na),
                meanPerformance_test_Mg=mean(performance_test_Mg),
                meanPerformance_test_carbon=mean(performance_test_carbon),
                meanPerformance_test_growth=mean(performance_test_growth))->result_List_sum

result_List_sum$analyzeName <- factor(result_List_sum$analyzeName,
                                      levels = c("mRNA", "protein"))

result_List_sum$model <- factor(result_List_sum$model,
                                levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# Repeated general figure
fig_test=ggplot(result_List_sum, aes(x=model, y=performance_test))+
  facet_grid(.~analyzeName)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw()+
  labs(title = "All conditions") + xlab("Model") + ylab("F1 performance on test data")

print(fig_test)
###*****************************


###*****************************
# generate tidy data frame related with individual results

result_List_sum %>%
  dplyr::group_by(analyzeName, TestTrainSubsetNo, model) %>%
  dplyr::select(performance_test_Na, performance_test_Mg, performance_test_carbon, performance_test_growth)%>%
  tidyr::gather(key = "tests", value = "performance_test", performance_test_Na:performance_test_growth)%>%
  dplyr::mutate(testFor=gsub(pattern = "performance_test_", replacement = "",x = tests))%>%
  dplyr::group_by(analyzeName,model,testFor)%>%
  dplyr::mutate(meanPerformance_test=mean(performance_test))->result_List_tidy

head(result_List_tidy)
###*****************************


###*****************************
# Fixing order of figures
# A
result_List_tidy$testFor = replace_fun(input_vector = result_List_tidy$testFor, 
                                       initialVal = "growth", 
                                       finalVal = "phase")

result_List_tidy$testFor <- factor(result_List_tidy$testFor,
                               levels = c("carbon", "Mg", "Na", "phase"))


# B
result_List_tidy$analyzeName<-factor(result_List_tidy$analyzeName)
result_List_tidy$analyzeName <- factor(result_List_tidy$analyzeName,
                                  levels = c("mRNA", "protein"))

# C
result_List_tidy$model <- factor(result_List_tidy$model,
                             levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# Generating Figures related with individual tests
fig_multi_violin=ggplot(result_List_tidy, aes(x=model, y=performance_test))+
  facet_grid(analyzeName~testFor)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw()+
  labs(title = "All conditions") + xlab("Model") + ylab("F1 performance on test data")

print(fig_multi_violin)

# Save figure
cowplot::save_plot(filename = "../b_figures/clustering_all_violin.jpeg", 
                   plot = fig_multi_violin, ncol = 2, nrow = 2)


fig_multi_line<-ggplot(result_List_tidy, aes(x=analyzeName, y=meanPerformance_test, group=model, colour=model))+
  facet_grid(.~testFor)+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title = "All conditions") + xlab("Model") + ylab("F1 performance on test data")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(fig_multi_line)
###*****************************


# 
# ###*****************************
# # Normalization for F1 Scores
# 
# # Meta MRNA
# ###*****************************
# # Parameters
# analyzeName="mRNA"
# pick_data="mrna"
# growthPhase="ExpAllPhase"
# testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
# ndivisionCost=55
# ndivisionGamma=31
# numRepeatsFor_TestTrainSubset_Choice=60
# doNotSave=0 # save the square table figures. 1 means DO NOT save
# costFunction="F1_final"
# 
# testConditionsCombined=paste0(testConditions,collapse = "_")
# ###*****************************
# 
# 
# ###*****************************
# # read the list to find file name
# timeStampFile<-read.csv(file = paste0("../b_results/","parametersModelFitMetafile",".csv")) #import file
# timeStampFile %>%
#   dplyr::filter(pick_data==get("pick_data")) %>%
#   dplyr::filter(growthPhase_names==get("growthPhase")) %>%
#   dplyr::filter(numRepeatsFor_TestTrainSubset_Choice==get("numRepeatsFor_TestTrainSubset_Choice")) %>%
#   dplyr::filter(ndivisionCost==get("ndivisionCost")) %>%
#   dplyr::filter(ndivisionGamma==get("ndivisionGamma")) %>%
#   dplyr::filter(testConditions==get("testConditionsCombined")) %>%
#   dplyr::filter(costFunction==get("costFunction"))->chosenDataSetInfo
# 
# 
# 
# if(nrow(chosenDataSetInfo)!=1){stop("one than one file selected")}
# 
# fileName=as.vector(chosenDataSetInfo$fileName)
# load(file = paste0("../b_results/",fileName,".RDA"))
# inputMetaDf_mRNA = inputMetaDf
# ###*****************************
# 
# # meta Protein
# ###*****************************
# # Parameters
# analyzeName="protein"
# pick_data="protein"
# growthPhase="ExpAllPhase"
# testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
# ndivisionCost=55
# ndivisionGamma=31
# numRepeatsFor_TestTrainSubset_Choice=60
# mtrylistRF=paste(seq(1,7),collapse = "_")
# doNotSave=0 # save the square table figures. 1 means DO NOT save
# costFunction="F1_final"
# 
# testConditionsCombined=paste0(testConditions,collapse = "_")
# ###*****************************
# 
# 
# ###*****************************
# # read the list to find file name
# timeStampFile<-read.csv(file = paste0("../b_results/","parametersModelFitMetafile",".csv")) #import file
# timeStampFile %>%
#   dplyr::filter(pick_data==get("pick_data")) %>%
#   dplyr::filter(growthPhase_names==get("growthPhase")) %>%
#   dplyr::filter(numRepeatsFor_TestTrainSubset_Choice==get("numRepeatsFor_TestTrainSubset_Choice")) %>%
#   dplyr::filter(ndivisionCost==get("ndivisionCost")) %>%
#   dplyr::filter(ndivisionGamma==get("ndivisionGamma")) %>%
#   dplyr::filter(testConditions==get("testConditionsCombined"))%>%
#   dplyr::filter(costFunction==get("costFunction"))->chosenDataSetInfo
# 
# 
# if(nrow(chosenDataSetInfo)!=1){stop("one than one file selected")}
# 
# fileName=as.vector(chosenDataSetInfo$fileName)
# load(file = paste0("../b_results/",fileName,".RDA"))
# inputMetaDf_protein = inputMetaDf
# ###*****************************
# 
# 
# ###*****************************
# inputMetaDf_mRNA %>%
#   dplyr::group_by(carbonSource)%>%
#   dplyr::summarize(NumCarbonSource=n())->inputMetaDf_mRNA_carbon
# 
# inputMetaDf_mRNA %>%
#   dplyr::group_by(growthPhase)%>%
#   dplyr::summarize(NumPhaseSource=n())->inputMetaDf_mRNA_phase
# 
# inputMetaDf_mRNA %>%
#   dplyr::group_by(Mg_mM_Levels)%>%
#   dplyr::summarize(NumMgSource=n())->inputMetaDf_mRNA_Mg
# 
# inputMetaDf_mRNA %>%
#   dplyr::group_by(Na_mM_Levels)%>%
#   dplyr::summarize(NumNaSource=n())->inputMetaDf_mRNA_Na
# ###*****************************
# 
# 
# ###*****************************
# inputMetaDf_protein %>%
#   dplyr::group_by(carbonSource)%>%
#   dplyr::summarize(NumCarbonSource=n())->inputMetaDf_protein_carbon
# 
# inputMetaDf_protein %>%
#   dplyr::group_by(growthPhase)%>%
#   dplyr::summarize(NumPhaseSource=n())->inputMetaDf_protein_phase
# 
# inputMetaDf_protein %>%
#   dplyr::group_by(Mg_mM_Levels)%>%
#   dplyr::summarize(NumMgSource=n())->inputMetaDf_protein_Mg
# 
# inputMetaDf_protein %>%
#   dplyr::group_by(Na_mM_Levels)%>%
#   dplyr::summarize(NumNaSource=n())->inputMetaDf_protein_Na
# ###*****************************
# 
# 
# 
# 
# randomF1Run<-function(nbins, loop, input)
# {
#   
#   numCut=nrow(input)-1
#   F1List=seq(from=0, to=1, length.out=nbins+1)
#   histVec=rep(0,nbins)
#   
#   for(counter01 in 1:loop)
#   {
#     confusionMatrix=matrix(nrow=nrow(input),ncol=nrow(input))
#     for(counter02 in 1: nrow(input))
#     {
#       cutPoints = sort(runif(numCut))
#       row_i=diff(c(0,cutPoints,1))
#       row_i_weight = input[[counter02,2]]*row_i
#       confusionMatrix[counter02,]=row_i_weight
#     }
#     
#     cm_diagonal=diag(confusionMatrix)
#     cm_rowSums=rowSums(confusionMatrix)
#     cm_colSums=colSums(confusionMatrix)
#     
#     precision=cm_diagonal/cm_rowSums
#     recall=cm_diagonal/cm_colSums
#     F1=2*precision*recall/(precision+recall)
#     macro_F1=mean(F1)
#     intervalNo=findInterval(x = macro_F1, vec = F1List)
# 
#     histVec[intervalNo] = histVec[intervalNo] + 1
#   }
#   return(histVec)
# }
# 
# nbin=200
# histVec=randomF1Run(nbin, 1000000, inputMetaDf_mRNA_carbon)
# x_axis=seq(from=1/nbin,to=1,by=1/nbin)-(1/2*1/nbin)
# plot(x=x_axis, y=histVec, xlim=c(0,1))
# q<-data.frame(x=x_axis,y=histVec)
# q %>% dplyr::mutate(prob=y/sum(y))
# 
# 
