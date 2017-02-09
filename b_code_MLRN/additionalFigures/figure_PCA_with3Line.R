# The machine learning script

# The steps for the script
# This part will be repeated many times with for loop
#     It take the data (mRNA or Protein), divide the data into 2 parts (intelligently) as training & control
#     This part will be repeated many times with for loop
#       In the training data, some of the data is trashed (randomly) in order to make the number of elements in each group comparible
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
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response/b_code_MLRN/"))} # mac computer
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
require("ggrepel")

# Machine learning
require("ape") # for pcoa (# the "pcoa" function)
require("vegan") # for pcoa (# the "vegdist" function)
require("e1071") # for svm
require("MASS") # to find matrix inverses
###*****************************


###*****************************
#Load Functions
source("dataChoiceFunctionSingle.R")
source("PCA_PCoA_func.R")
source("../a_code_dataPreperation_RNA&Protein/data_naming_functions.R")
###*****************************


###*****************************
# Find the csv files that need to be imported
dataName=name_data(initialValue=c("resDf"), # can be c("genes0.05","genes_P0.05Fold2","resDf")
                   dataType = "mrna", # can be "rna", "mrna", "protein", "protein_wo_NA"
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
                   carbonSourceVector = "S", # can be any sub combination of "SYAN"
                   MgLevelVector = c("baseMg"), # can be "lowMg","baseMg","highMg" // "allMg"
                   NaLevelVector = c("baseNa"), # can be "baseNa","highNa" // "allNa"
                   growthPhaseVector = c("exponential","stationary","late_stationary"), # can be "exponential","stationary","late_stationary" // "allPhase"
                   filterGenes = "noFilter", # can be "noFilter", "meanFilter", "maxFilter", "sdFilter" 
                   threshold=NA, # the threshold value for "meanFilter", "maxFilter", "sdFilter"
                   roundData=TRUE,
                   sumTechnicalReplicates=TRUE,
                   deSeqSfChoice="p1Sf", # can be "regSf", "p1Sf"
                   normalizationMethodChoice= "vst", # can be "vst", "rlog", "log10", "noNorm"
                   test_for = "noTest")  # works only if normalizationMethodChoice == noNorm
# c("Mg_mM_Levels", "Na_mM_Levels", "growthPhase", "carbonSource", "noTest")
dataNameDF=as.data.frame(dataName[1])

metaDataName=dataNameDF
metaDataName$objectName.initial="metaData"
treeDataName=dataNameDF
treeDataName$objectName.initial="treeData"
heatMapName=dataNameDF
heatMapName$objectName.initial="heatMap"

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
nModels=1000 # rigt now it is 1
testConditions=c("growthPhase") # different combinations that we will look into 
# Options carbonSource, growthPhase, Mg_mM_Levels, Na_mM_Levels
dimReductionType="PCA" # Can be PCA or PCoA
type_svmChoice="C-classification" #Can be "C-classification" but not "eps-regression" 
kernel_typeChoice="radial"
###*****************************


###*****************************
# Make the names look better
# Rename Updated columns
temp<-list("Exponential"="exponential",
           "Late-Stationary"="late_stationary", 
           "Stationary"="stationary")
levels(condition$growthPhase)<-temp

temp<-list("Glucose"="glucose",
           "Glycerol"="glycerol",
           "Gluconate"="gluconate",
           "Lactate"="lactate" )
levels(condition$carbonSource)<-temp

temp<-list("Low Mg"="lowMg",
           "Base Mg"="baseMg",
           "High Mg"="highMg")
levels(condition$Mg_mM_Levels)<-temp

temp<-list("Base Na"="baseNa",
           "High Na"="highNa")
levels(condition$Na_mM_Levels)<-temp
###*****************************


###*****************************
# Arrange and rename order of factors 

# Initial name list for different variables (in correct order)
growthPhaseVector=c("Exponential","Stationary","Late-Stationary")
carbonSourceVector=c("Glucose","Glycerol","Gluconate","Lactate")
Mg_mM_LevelsVector=c("Low Mg","Base Mg","High Mg")
Na_mM_LevelsVector=c("Base Na","High Na")

# Updated name list for different variables (in correct order)
growthPhaseVector_Short=c("Exp","Sta","Lt.Sta")
carbonSourceVector_Short=c("G.se","Gly","G.te","Lac")
Mg_mM_LevelsVector_Short=c("L.Mg","B.Mg","H.Mg")
Na_mM_LevelsVector_Short=c("B.Na","H.Na")

# Generated vectior lists associated with vectors and short vectors
vectorList=list(growthPhase=growthPhaseVector,
                carbonSource=carbonSourceVector,
                Mg_mM_Levels=Mg_mM_LevelsVector,
                Na_mM_Levels=Na_mM_LevelsVector)

vectorList_Short=list(growthPhase_Short=growthPhaseVector_Short,
                      carbonSource_Short=carbonSourceVector_Short,
                      Mg_mM_Levels_Short=Mg_mM_LevelsVector_Short,
                      Na_mM_Levels_Short=Na_mM_LevelsVector_Short)

# Put column labels in order 
condition$growthPhase <- factor(condition$growthPhase, 
                                levels=growthPhaseVector)
condition$carbonSource <- factor(condition$carbonSource, 
                                 levels=carbonSourceVector)
condition$Mg_mM_Levels <- factor(condition$Mg_mM_Levels, 
                                 levels=Mg_mM_LevelsVector)
condition$Na_mM_Levels <- factor(condition$Na_mM_Levels, 
                                 levels=Na_mM_LevelsVector)

# Generate updated columns
condition %>%
  dplyr::mutate(growthPhase_Short=growthPhase,
                carbonSource_Short=carbonSource,
                Mg_mM_Levels_Short=Mg_mM_Levels,
                Na_mM_Levels_Short=Na_mM_Levels)->condition


# Rename Updated columns
temp<-as.list(vectorList$growthPhase)
names(temp)<-vectorList_Short$growthPhase
levels(condition$growthPhase_Short)<-temp

temp<-as.list(vectorList$carbonSource)
names(temp)<-vectorList_Short$carbonSource_Short
levels(condition$carbonSource_Short)<-temp

temp<-as.list(vectorList$Mg_mM_Levels)
names(temp)<-vectorList_Short$Mg_mM_Levels_Short
levels(condition$Mg_mM_Levels_Short)<-temp

temp<-as.list(vectorList$Na_mM_Levels)
names(temp)<-vectorList_Short$Na_mM_Levels_Short
levels(condition$Na_mM_Levels_Short)<-temp
###*****************************


###*****************************
# Generate "conditionInvestigated" and "conditionInvestigated_Short" data column in meta data
inputMetaDf=condition
if(length(testConditions)!=1)
{
  inputMetaDf$conditionInvestigated <- apply( inputMetaDf[,testConditions] , 1 , paste , collapse = "_") # generate the "conditionInvestigated" column
  expandedGrid<-expand.grid(vectorList[paste0(testConditions)]) # generate an expandedGrid DF 
  expandedGrid$conditionInvestigatedVector <- apply( expandedGrid[,paste0(testConditions)] , 1 , paste , collapse = "_") # generate a "conditionInvestigated" column in pandedGrid DF
  conditionInvestigatedVector=expandedGrid$conditionInvestigatedVector # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated" column)
  
  inputMetaDf$conditionInvestigated_Short <- apply( inputMetaDf[,paste0(testConditions,"_Short")] , 1 , paste , collapse = "_") # generate the "conditionInvestigated_Short" column
  expandedGrid_Short<-expand.grid(vectorList_Short[paste0(testConditions,"_Short")]) # generate an expandedGrid_Short DF 
  expandedGrid_Short$conditionInvestigatedVector_Short <- apply( expandedGrid_Short[,paste0(testConditions,"_Short")] , 1 , paste , collapse = "_") # generate a "conditionInvestigated_Short" column in pandedGrid_Short DF
  conditionInvestigatedVector_Short=expandedGrid_Short$conditionInvestigatedVector_Short # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated_Short" column)
  
  # Import the order of labels to "conditionInvestigated" column
  inputMetaDf$conditionInvestigated <- factor(inputMetaDf$conditionInvestigated, 
                                              levels=conditionInvestigatedVector)
  inputMetaDf$conditionInvestigated<-droplevels(inputMetaDf$conditionInvestigated)
  
  # Import the order of labels to "conditionInvestigated_Short" column
  inputMetaDf$conditionInvestigated_Short <- factor(inputMetaDf$conditionInvestigated_Short, 
                                                    levels=conditionInvestigatedVector_Short)
  inputMetaDf$conditionInvestigated_Short<-droplevels(inputMetaDf$conditionInvestigated_Short)
}


if(length(testConditions)==1)
{
  inputMetaDf%>% 
    dplyr::mutate_(conditionInvestigated=testConditions)->inputMetaDf
  inputMetaDf%>% 
    dplyr::mutate_(conditionInvestigated_Short=paste0(testConditions,"_Short"))->inputMetaDf
  
  expandedGrid=as.data.frame(levels(inputMetaDf$conditionInvestigated))
  colnames(expandedGrid)<-testConditions
  expandedGrid[[testConditions]]<-factor(expandedGrid[[testConditions]], 
                                         levels=as.vector(expandedGrid[[testConditions]]))
  expandedGrid %>%
    dplyr::mutate_(conditionInvestigatedVector=testConditions)->expandedGrid
  
  expandedGrid_Short=as.data.frame(levels(inputMetaDf$conditionInvestigated_Short))
  colnames(expandedGrid_Short)<-paste0(testConditions,"_Short")
  expandedGrid_Short[[paste0(testConditions,"_Short")]]<-factor(expandedGrid_Short[[paste0(testConditions,"_Short")]], 
                                                                levels=as.vector(expandedGrid_Short[[paste0(testConditions,"_Short")]]))
  expandedGrid_Short %>%
    dplyr::mutate_(conditionInvestigatedVector_Short=paste0(testConditions,"_Short"))->expandedGrid_Short
  
  conditionInvestigatedVector=expandedGrid$conditionInvestigatedVector # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated" column)
  conditionInvestigatedVector_Short=expandedGrid_Short$conditionInvestigatedVector_Short # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated_Short"
  
  # Import the order of labels to "conditionInvestigated" column
  inputMetaDf$conditionInvestigated <- factor(inputMetaDf$conditionInvestigated, 
                                              levels=conditionInvestigatedVector)
  inputMetaDf$conditionInvestigated<-droplevels(inputMetaDf$conditionInvestigated)
  
  # Import the order of labels to "conditionInvestigated_Short" column
  inputMetaDf$conditionInvestigated_Short <- factor(inputMetaDf$conditionInvestigated_Short, 
                                                    levels=conditionInvestigatedVector_Short)
  inputMetaDf$conditionInvestigated_Short<-droplevels(inputMetaDf$conditionInvestigated_Short)
}

factorOrder<-levels(inputMetaDf$conditionInvestigated)
factorOrder_Short<-levels(inputMetaDf$conditionInvestigated_Short)
###*****************************


###*****************************
# Preperation of Data for PCA and PCoA
# 1.) Remove rows (genes) with 0 sd (PCA fails without this)
# 2.) transpose the data frame
zerosdList=which(apply(mainDataFrame,1,sd)==0)
if(length(zerosdList)!=0){mainDataFrame=mainDataFrame[-zerosdList,]} # get rid of 0 sd data

mainDataFrame=t(mainDataFrame)
###*****************************


###*****************************
# new data frame with missing gene
mainDataFrame[,-c(1:100)]->mainDataFrameMG
###*****************************


###*****************************
# Do PCA or PCoA
if(dimReductionType=="PCA")
{
  mapped_DF=pca_analyze_wrong(mainDataFrame, inputMetaDf)
  mapped_DF_MG=pca_analyze_wrong(mainDataFrameMG, inputMetaDf)
}
if(dimReductionType=="PCoA")
{
  mapped_DF=pcoa_analyze_wrong(mainDataFrame, inputMetaDf)
  mapped_DF_MG=pcoa_analyze_wrong(mainDataFrameMG, inputMetaDf)
}
###*****************************


###*****************************
axisNames=levels(mapped_DF$conditionInvestigated) # generate the axis names for the figure at the end
###*****************************

###*****************************

set.seed(14159)
train_and_plot_svm <- function(train, kernel = "radial", type ="C", cost, gamma) {
  fit <- e1071::svm(as.factor(value) ~ x + y, data = train, kernel = kernel, type = type, cost = cost)
  grid <- expand.grid (x = seq(from = -100, to = 100, length = 100), y = seq(from = -100, to = 100, length = 100))
  decisionValues1 <- as.vector(attributes(predict(fit, grid, decision.values = TRUE))$decision[,1])
  decisionValues2 <- as.vector(attributes(predict(fit, grid, decision.values = TRUE))$decision[,2])
  decisionValues3 <- as.vector(attributes(predict(fit, grid, decision.values = TRUE))$decision[,3])
  p <- predict(fit, grid)
  grid$value <- p
  grid$z1 <- decisionValues1
  grid$z2 <- decisionValues2
  grid$z3 <- decisionValues3
  p <- ggplot() + 
    stat_contour(data = grid, aes(x = x, y = y, z = z1), breaks = c(0),color="#a6cee3") +
    stat_contour(data = grid, aes(x = x, y = y, z = z2), breaks = c(0),color="#1f78b4") +
    stat_contour(data = grid, aes(x = x, y = y, z = z3), breaks = c(0),color="#b2df8a") 
  p <- p + geom_point(data = train, aes(x, y, colour = as.factor(value)), alpha = 0.8, size=3)
  p <- p + scale_colour_manual(values = c("#8da0cb","#fc8d62","#66c2a5"))
  p <- p + geom_text_repel(data = train, aes(x=x, y=y, label = dataSet), size = 3,fontface = 'plain',force = 2,point.padding = unit(0.5, 'lines'),colour="grey50", segment.color="grey")
  p <- p + scale_x_continuous(limits=c(-70,100)) + scale_y_continuous(limits=c(-90,50)) + 
    guides(color = guide_legend(title = "Growth Phase"))+
    xlab("Axis.1")+ylab("Axis.2")
}

mapped_DF %>%
  dplyr::mutate(x=Axis.1,y=Axis.2,value=growthPhase)->mapped_DF

fig01<-train_and_plot_svm(train=mapped_DF, kernel = "radial", type ="C", cost=20, gamma=1/2)
#plot(fig01)

cowplot::save_plot(filename = "../b_figures/PCA_FigA.pdf",plot = fig01,ncol=1.5,nrow = 1.5,base_aspect_ratio = 1.5)



set.seed(14159)
train_and_plot_svm <- function(train1, train2, kernel = "radial", type ="C", cost, gamma) {
  fit1 <- e1071::svm(as.factor(value) ~ x + y, data = train1, kernel = kernel, type = type, cost = cost)
  fit2 <- e1071::svm(as.factor(value) ~ x + y, data = train2, kernel = kernel, type = type, cost = cost)
  grid <- expand.grid (x = seq(from = -100, to = 100, length = 100), y = seq(from = -100, to = 100, length = 100))
  decisionValues1 <- as.vector(attributes(predict(fit1, grid, decision.values = TRUE))$decision[,1])
  decisionValues2 <- as.vector(attributes(predict(fit1, grid, decision.values = TRUE))$decision[,2])
  decisionValues3 <- as.vector(attributes(predict(fit1, grid, decision.values = TRUE))$decision[,3])
  decisionValues4 <- as.vector(attributes(predict(fit2, grid, decision.values = TRUE))$decision[,1])
  decisionValues5 <- as.vector(attributes(predict(fit2, grid, decision.values = TRUE))$decision[,2])
  decisionValues6 <- as.vector(attributes(predict(fit2, grid, decision.values = TRUE))$decision[,3])
  p1 <- predict(fit1, grid)
  grid$value <- p1
  grid$z1 <- decisionValues1
  grid$z2 <- decisionValues2
  grid$z3 <- decisionValues3
  grid$z4 <- decisionValues4
  grid$z5 <- decisionValues5
  grid$z6 <- decisionValues6
  p <- ggplot() +
    stat_contour(data = grid, aes(x = x, y = y, z = z1), breaks = c(0),color="#a6cee3") +
    stat_contour(data = grid, aes(x = x, y = y, z = z2), breaks = c(0),color="#1f78b4") +
    stat_contour(data = grid, aes(x = x, y = y, z = z3), breaks = c(0),color="#b2df8a") +
    stat_contour(data = grid, aes(x = x, y = y, z = z4), breaks = c(0),color="#a6cee3",linetype=2) +
    stat_contour(data = grid, aes(x = x, y = y, z = z5), breaks = c(0),color="#1f78b4",linetype=2) +
    stat_contour(data = grid, aes(x = x, y = y, z = z6), breaks = c(0),color="#b2df8a",linetype=2)
  p <- p + geom_point(data = train1, aes(x, y, colour = as.factor(value)), alpha = 0.8, size=3)
  p <- p + scale_colour_manual(values = c("#8da0cb","#fc8d62","#66c2a5"))
  p <- p + geom_text_repel(data = train1, aes(x=x, y=y, label = dataSet), size = 3,fontface = 'plain',force = 2,point.padding = unit(0.5, 'lines'),colour="grey50", segment.color="grey")
  p <- p + scale_x_continuous(limits=c(-70,100)) + scale_y_continuous(limits=c(-90,50)) + 
    guides(color = guide_legend(title = "Growth Phase"))+
    xlab("Axis.1")+ylab("Axis.2")
}
mapped_DF %>%
  dplyr::mutate(x=Axis.1,y=Axis.2,value=growthPhase)->mapped_DF
mapped_DF_MG %>%
  dplyr::mutate(x=Axis.1,y=Axis.2,value=growthPhase)->mapped_DF_MG

fig02<-train_and_plot_svm(train1=mapped_DF, train2=mapped_DF_MG, kernel = "radial", type ="C", cost=20, gamma=1/2)
#plot(fig02)

cowplot::save_plot(filename = "../b_figures/PCA_FigB.pdf",plot = fig02,ncol=1.5,nrow = 1.5,base_aspect_ratio = 1.5)
