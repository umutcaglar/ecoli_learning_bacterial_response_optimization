# dintinct test condition performances of mRNA and protein 

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
###*****************************


###*****************************
#Load Functions
source("../a_code_dataPreperation_RNA&Protein/replace_fun.R")
###*****************************


###*****************************
# read data
winnerModels_mRNA_carbon = read.csv(file = "../b_results/model_performance_mRNA_carbon.csv")
winnerModels_mRNA_growth = read.csv(file = "../b_results/model_performance_mRNA_growth.csv")
winnerModels_mRNA_Mg = read.csv(file = "../b_results/model_performance_mRNA_Mg.csv")
winnerModels_mRNA_Na = read.csv(file = "../b_results/model_performance_mRNA_Na.csv")
winnerModels_protein_carbon = read.csv(file = "../b_results/model_performance_protein_carbon.csv")
winnerModels_protein_growth = read.csv(file = "../b_results/model_performance_protein_growth.csv")
winnerModels_protein_Mg = read.csv(file = "../b_results/model_performance_protein_Mg.csv")
winnerModels_protein_Na = read.csv(file = "../b_results/model_performance_protein_Na.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_mRNA_carbon,
                               winnerModels_mRNA_growth,
                               winnerModels_mRNA_Mg,
                               winnerModels_mRNA_Na,
                               winnerModels_protein_carbon,
                               winnerModels_protein_growth,
                               winnerModels_protein_Mg,
                               winnerModels_protein_Na)

winnerModels%>%
  tidyr::separate(analyzeName, c("pick_data", "tested_for"), "_")->winnerModels

winnerModels$tested_for <- factor(winnerModels$tested_for,
                                   levels = c("carbon", "growth", "Mg", "Na"))

winnerModels$analyzeName <- factor(winnerModels$pick_data,
                                   levels = c("mRNA", "protein"))

winnerModels$model <- factor(winnerModels$model,
                             levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# generate the increase in success figure
fig01<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
  facet_grid(pick_data ~ tested_for)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw(base_size=20)+
  xlab("Model") + ylab("F1 Performance on Test Data")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

print(fig01)

# fig02<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
#   facet_grid(.~analyzeName)+
#   geom_violin(aes(fill=model, color=model))+
#   geom_point(aes(x=model, y=meanPerformance_test))+
#   theme_bw()+
#   labs(title = "All conditions")
# 
# print(fig02)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/distinctTests_mRNA_Protein.pdf", 
                   plot = fig01, ncol = 3, nrow = 2.3)

cowplot::save_plot(filename = "../b_figures/distinctTests_mRNA_Protein_ppt.pdf", 
                   plot = fig01, ncol = 3, nrow = 1.4*3/2)
###*****************************



# load square plots
###*****************************

# mRNA
load("../b_figures/fig_obj_mRNA_carbon.Rda")
figComb_mrna_carbon<-figComb

load("../b_figures/fig_obj_mRNA_growth.Rda")
figComb_mrna_growth<-figComb

load("../b_figures/fig_obj_mRNA_Mg.Rda")
figComb_mrna_Mg<-figComb

load("../b_figures/fig_obj_mRNA_Na.Rda")
figComb_mrna_Na<-figComb

# Protein
load("../b_figures/fig_obj_protein_carbon.Rda")
figComb_protein_carbon<-figComb

load("../b_figures/fig_obj_protein_growth.Rda")
figComb_protein_growth<-figComb

load("../b_figures/fig_obj_protein_Mg.Rda")
figComb_protein_Mg<-figComb

load("../b_figures/fig_obj_protein_Na.Rda")
figComb_protein_Na<-figComb
###*****************************


###*****************************
# Combine Plots

fig02a<-cowplot::plot_grid(figComb_mrna_carbon, figComb_mrna_Mg, figComb_mrna_Na, figComb_mrna_growth, 
                   figComb_protein_carbon, figComb_protein_Mg, figComb_protein_Na, figComb_protein_growth, 
                   nrow = 2, ncol = 4, scale = .9, labels = c("A","B","C","D","E","F","G","I"))

print(fig02a)


fig02b<-cowplot::plot_grid(figComb_mrna_carbon, figComb_protein_carbon,
                           figComb_mrna_Mg, figComb_protein_Mg,
                           figComb_mrna_Na, figComb_protein_Na, 
                           figComb_mrna_growth, figComb_protein_growth,
                           nrow = 4, ncol = 2, scale = .9, labels = c("A","B","C","D","E","F","G","I"))

print(fig02b)


cowplot::save_plot(filename = "../b_figures/distinctTestsConfMatrix_mRNA_Protein_ppt.jpeg", 
                   plot = fig02a, ncol = 4, nrow = 2)

cowplot::save_plot(filename = "../b_figures/distinctTestsConfMatrix_mRNA_Protein.jpeg", 
                   plot = fig02b, ncol = 2, nrow = 4)

###*****************************


###*****************************
# Normalization for F1 Scores

# Meta MRNA
###*****************************
# Parameters
analyzeName="mRNA"
pick_data="mrna"
growthPhase="ExpAllPhase"
testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
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
  dplyr::filter(testConditions==get("testConditionsCombined")) %>%
  dplyr::filter(costFunction==get("costFunction"))->chosenDataSetInfo



if(nrow(chosenDataSetInfo)!=1){stop("one than one file selected")}

fileName=as.vector(chosenDataSetInfo$fileName)
load(file = paste0("../b_results/",fileName,".RDA"))
inputMetaDf_mRNA = inputMetaDf
###*****************************

# meta Protein
###*****************************
# Parameters
analyzeName="protein"
pick_data="protein"
growthPhase="ExpAllPhase"
testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
ndivisionCost=55
ndivisionGamma=31
numRepeatsFor_TestTrainSubset_Choice=60
mtrylistRF=paste(seq(1,7),collapse = "_")
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


if(nrow(chosenDataSetInfo)!=1){stop("one than one file selected")}

fileName=as.vector(chosenDataSetInfo$fileName)
load(file = paste0("../b_results/",fileName,".RDA"))
inputMetaDf_protein = inputMetaDf
###*****************************


###*****************************
inputMetaDf_mRNA %>%
  dplyr::group_by(carbonSource)%>%
  dplyr::summarize(NumCarbonSource=n())->inputMetaDf_mRNA_carbon

inputMetaDf_mRNA %>%
  dplyr::group_by(growthPhase)%>%
  dplyr::summarize(NumPhaseSource=n())->inputMetaDf_mRNA_phase

inputMetaDf_mRNA %>%
  dplyr::group_by(Mg_mM_Levels)%>%
  dplyr::summarize(NumMgSource=n())->inputMetaDf_mRNA_Mg

inputMetaDf_mRNA %>%
  dplyr::group_by(Na_mM_Levels)%>%
  dplyr::summarize(NumNaSource=n())->inputMetaDf_mRNA_Na
###*****************************


###*****************************
inputMetaDf_protein %>%
  dplyr::group_by(carbonSource)%>%
  dplyr::summarize(NumCarbonSource=n())->inputMetaDf_protein_carbon

inputMetaDf_protein %>%
  dplyr::group_by(growthPhase)%>%
  dplyr::summarize(NumPhaseSource=n())->inputMetaDf_protein_phase

inputMetaDf_protein %>%
  dplyr::group_by(Mg_mM_Levels)%>%
  dplyr::summarize(NumMgSource=n())->inputMetaDf_protein_Mg

inputMetaDf_protein %>%
  dplyr::group_by(Na_mM_Levels)%>%
  dplyr::summarize(NumNaSource=n())->inputMetaDf_protein_Na
###*****************************




randomF1Run<-function(loop, input)
{
  
  numCut=nrow(input)-1
  F1List=rep(NA,loop)
  
  for(counter01 in 1:loop)
  {
    confusionMatrix=matrix(nrow=nrow(input),ncol=nrow(input))
    for(counter02 in 1: nrow(input))
    {
      cutPoints = sort(runif(numCut))
      row_i=diff(c(0,cutPoints,1))
      row_i_weight = input[[counter02,2]]*row_i
      confusionMatrix[counter02,]=row_i_weight
    }
    
    cm_diagonal=diag(confusionMatrix)
    cm_rowSums=rowSums(confusionMatrix)
    cm_colSums=colSums(confusionMatrix)
    
    precision=cm_diagonal/cm_rowSums
    recall=cm_diagonal/cm_colSums
    F1=2*precision*recall/(precision+recall)
    macro_F1=mean(F1)
    F1List[counter01]=macro_F1
  }
  return(F1List)
}

histVec=randomF1Run(100000, inputMetaDf_mRNA_carbon)
a=hist(histVec,xlim = c(0,1),breaks=100)
a$counts


