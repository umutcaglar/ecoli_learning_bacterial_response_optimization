# Sunburst R

# The aim of the file is to built a sunburst graph that represents my data
# From inside to outside it will have carbon source growth time 

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
 {setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response/text/figures/figrue_preperation/"))} # mac computersldk
###*****************************


###*****************************
# REQUIRED LIBRARIES
# Data tracking
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
require("scales") # Utilities for scales and formatting
###*****************************


###*****************************
#Load Functions
source("../../../b_code_MLRN_svm/dataChoiceFunctionSingle.R")
source("../../../a_code_dataPreperation_RNA&Protein/data_naming_functions.R")
source("../../../a_code_dataPreperation_RNA&Protein/replace_fun.R")
###*****************************


###*****************************
dataTypeVector=c("protein", "mrna")

for (counter01 in 1:length(dataTypeVector))
{
  # Find the csv files that need to be imported
  dataName=name_data(initialValue=c("resDf"), # can be c("genes0.05","genes_P0.05Fold2","resDf")
                     dataType = dataTypeVector[counter01], # can be "rna", "mrna", "protein", "protein_wo_NA", "comb_mrna_protein"
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
  
  condition=read.csv(file = paste0("../../../a_results/",metaDataName,".csv"),header = TRUE)
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
  assign(x = paste0("condition_",dataTypeVector[counter01]), value = condition)
  ###*****************************
}


###*****************************
condition_mrna %>%
  dplyr::select(dataSet,carbonSource,growthPhase_Short,Mg_mM_Levels_Short,Na_mM_Levels_Short) %>%
  dplyr::mutate(mrna="mrna_sample")->condition_mrna
condition_protein %>%
  dplyr::select(dataSet,carbonSource,growthPhase_Short,Mg_mM_Levels_Short,Na_mM_Levels_Short)%>%
  dplyr::mutate(protein="protein_sample")->condition_protein

condition<-merge(condition_mrna,condition_protein,all = T)

condition$mrna[is.na(condition$mrna)] <- "missing_mrna"
condition$protein[is.na(condition$protein)] <- "missing_protein"

mrna_vector=c("mrna_sample","missing_mrna")
protein_vector=c("protein_sample","missing_protein")

condition$mrna <- factor(condition$mrna, 
                         levels=mrna_vector)
condition$protein <- factor(condition$protein, 
                            levels=protein_vector)
###*****************************


###*****************************
condition %>%
  dplyr::group_by(carbonSource,growthPhase_Short,Mg_mM_Levels_Short,Na_mM_Levels_Short,dataSet,mrna,protein)%>%
  dplyr::summarise(numSamples=n())->conditionSummary
###*****************************


# ***BEGINNING OF FIGURE***
###*****************************

###*****************************
# Angle Function
compute_angle = function(perc){
  angle = -1
  if(perc < 0.25) # 1st q [90,0]
    angle = 90 - (perc/0.25) * 90
  else if(perc < 0.5) # 2nd q [0, -90]
    angle = (perc-0.25) / 0.25 * -90
  else if(perc < 0.75) # 3rd q [90, 0]
    angle = 90 - ((perc-0.5) / 0.25 * 90)
  else if(perc < 1.00) # last q [0, -90]
    angle = ((perc -0.75)/0.25) * -90
  # Or even more compact, but less readable
  if(perc < 0.5) # 1st half [90, -90]
    angle = (180 - (perc/0.5) * 180) - 90
  else # 2nd half [90, -90]
    angle = (90 - ((perc - 0.5)/0.5) * 180)
  return(angle)
}
###*****************************


###*****************************
# Colors
carbonSourceColorVector=c("#bcbddc","#9e9ac8","#807dba","#6a51a3")
growthPhaseColorVector=c("#bae4b3","#74c476","#238b45")
Mg_mM_LevelsColorVector=c("#bdd7e7","#6baed6","#2171b5")
Na_mM_LevelsColorVector=c("#fdbe85","#fd8d3c")
mrna_colors=c("yellow3","white")
protein_colors=c("grey80","white")

cols=c(carbonSourceColorVector,growthPhaseColorVector,Mg_mM_LevelsColorVector,Na_mM_LevelsColorVector,mrna_colors,protein_colors)
colOrder_Short<-c(carbonSourceVector,growthPhaseVector_Short,Mg_mM_LevelsVector_Short,Na_mM_LevelsVector_Short,mrna_vector,protein_vector)
colOrder_Limits<-c(carbonSourceVector,growthPhaseVector_Short,Mg_mM_LevelsVector_Short,Na_mM_LevelsVector_Short)
colOrder<-c(carbonSourceVector,growthPhaseVector,Mg_mM_LevelsVector,Na_mM_LevelsVector,mrna_vector,protein_vector)
names(cols)<-colOrder_Short
###*****************************


###*****************************
# Data Frames
total_data = sum(conditionSummary$numSamples)

conditionSummary %>% 
  dplyr::group_by() %>% 
  dplyr::summarize(numSamples=sum(numSamples))->firstLevel


conditionSummary %>% 
  dplyr::group_by(carbonSource) %>% 
  dplyr::summarize(numSamples=sum(numSamples)) %>%
  dplyr::group_by() %>%
  dplyr::mutate(running=cumsum(numSamples), pos=running - numSamples/2) %>%
  dplyr::group_by(1:n()) %>% # to compute row by row
  dplyr::mutate(angle=compute_angle((running - numSamples/2) / total_data))-> secondLevel


conditionSummary %>% 
  dplyr::group_by(carbonSource, growthPhase_Short) %>% 
  dplyr::summarize(numSamples=sum(numSamples)) %>%
  dplyr::group_by() %>%
  dplyr::mutate(running=cumsum(numSamples), pos=running - numSamples/2)%>%
  dplyr::group_by(1:n()) %>% # to compute row by row
  dplyr::mutate(angle=compute_angle((running - numSamples/2) / total_data))-> thirdLevel

conditionSummary %>% 
  dplyr::group_by(carbonSource, growthPhase_Short, Mg_mM_Levels_Short) %>% 
  dplyr::summarize(numSamples=sum(numSamples)) %>%
  dplyr::group_by() %>%
  dplyr::mutate(running=cumsum(numSamples), pos=running - numSamples/2)%>%
  dplyr::group_by(1:n()) %>% # to compute row by row
  dplyr::mutate(angle=compute_angle((running - numSamples/2) / total_data))-> fourthLevel

conditionSummary %>% 
  dplyr::group_by(carbonSource, growthPhase_Short, Mg_mM_Levels_Short, Na_mM_Levels_Short) %>% 
  dplyr::summarize(numSamples=sum(numSamples)) %>%
  dplyr::group_by() %>%
  dplyr::mutate(running=cumsum(numSamples), pos=running - numSamples/2)%>%
  dplyr::group_by(1:n()) %>% # to compute row by row
  dplyr::mutate(angle=compute_angle((running - numSamples/2) / total_data))-> fifthLevel


conditionSummary %>% 
  dplyr::group_by(carbonSource, growthPhase_Short, Mg_mM_Levels_Short, Na_mM_Levels_Short, dataSet, mrna) %>% 
  dplyr::summarize(numSamples=sum(numSamples)) %>%
  dplyr::group_by() %>%
  dplyr::mutate(running=cumsum(numSamples), pos=running - numSamples/2)%>%
  dplyr::group_by(1:n()) %>% # to compute row by row
  dplyr::mutate(angle=compute_angle((running - numSamples/2) / total_data))-> sixthLevel

conditionSummary %>% 
  dplyr::group_by(carbonSource, growthPhase_Short, Mg_mM_Levels_Short, Na_mM_Levels_Short, dataSet, protein) %>% 
  dplyr::summarize(numSamples=sum(numSamples)) %>%
  dplyr::group_by() %>%
  dplyr::mutate(running=cumsum(numSamples), pos=running - numSamples/2)%>%
  dplyr::group_by(1:n()) %>% # to compute row by row
  dplyr::mutate(angle=compute_angle((running - numSamples/2) / total_data))-> seventhLevel
###*****************************


###*****************************
sunburst_0 = ggplot(firstLevel) # Just a foundation
sunburst_1 = 
  sunburst_0 + 
  geom_bar(data=firstLevel, aes(x=1, y=total_data), fill='lightgrey', stat='identity') +
  geom_text(aes(x=0.5, y=total_data/4, label=paste("Data")), fontface="bold", size =7)
sunburst_1

sunburst_2 = sunburst_1 +
  geom_bar(data=secondLevel,
           aes(x=2.25, y=numSamples, fill=carbonSource, stroke=3),
           color='white', position='stack', stat='identity', size=0.6, width = 1.5) + 
  geom_text(data=secondLevel, aes(label=carbonSource, x=2.25, y=pos, angle=angle), fontface="bold")
sunburst_2

sunburst_3 = sunburst_2 +
  geom_bar(data=thirdLevel,
           aes(x=3.5, y=numSamples, fill=growthPhase_Short, stroke=3),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=thirdLevel, aes(label=growthPhase_Short, x=3.5, y=pos, angle=angle), fontface="bold")
sunburst_3

sunburst_4 = sunburst_3 +
  geom_bar(data=fourthLevel,
           aes(x=4.5, y=numSamples, fill=Mg_mM_Levels_Short, stroke=3),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=fourthLevel, aes(label=Mg_mM_Levels_Short, x=4.5, y=pos, angle=angle), fontface="bold")
sunburst_4

sunburst_5 = sunburst_4 +
  geom_bar(data=fifthLevel,
           aes(x=5.5, y=numSamples, fill=Na_mM_Levels_Short, stroke=3),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=fifthLevel, aes(label=Na_mM_Levels_Short, x=5.5, y=pos, angle=angle), fontface="bold")
sunburst_5


sunburst_6 = sunburst_5 +
  geom_bar(data=sixthLevel,
           aes(x=7.5, y=numSamples, fill=factor(mrna), stroke=0),
           color='white', position='stack', stat='identity', size=0.1, width = 0.75) 
sunburst_6


sunburst_7 = sunburst_6 +
  geom_bar(data=seventhLevel,
           aes(x=8.375, y=numSamples, fill=factor(protein), stroke=0),
           color='white', position='stack', stat='identity', size=0.1, width = 0.75) 
sunburst_7


finalFig=sunburst_7+
  scale_fill_manual(values = cols, # matches variables with colors
                    breaks = colOrder_Short, # order of variables
                    labels = colOrder, # rename variables 
                    limits = colOrder_Limits, # decide which variables will be in legend
                    guide = guide_legend(byrow = TRUE,
                                         nrow=2,
                                         title.position = "top"))+
  coord_polar('y')+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position="bottom",
        legend.title=element_blank())
finalFig
###*****************************


###*****************************
# Dummy figure
dummy_data<-data.frame(x=c(1,2,3),y=c(3,5,5),cond=c("mrna_sample","protein_sample","protein_sample"))
dummy_fig<-ggplot(data = dummy_data, mapping = aes(x=x, fill=cond))+ geom_bar()+
  scale_fill_manual(values = c("mrna_sample"="yellow3","protein_sample"="grey80"), # matches variables with colors
                    breaks = c("mrna_sample","protein_sample"),
                    labels = c("mRNA sample","Protein sample"))+ # order of variables
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank())
dummy_fig
###*****************************


###*****************************
# Save Figure
cowplot::save_plot(filename = "figure1.png", plot = finalFig, ncol = 3.15, nrow = 3.15, dpi=300)
cowplot::save_plot(filename = "figure_dummy.png", plot = dummy_fig, ncol = 1, nrow = 1, dpi=300)
###*****************************