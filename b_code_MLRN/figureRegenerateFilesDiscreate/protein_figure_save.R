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
###*****************************


###*****************************
#Load Functions
source("../a_code_dataPreperation_RNA&Protein/replace_fun.R")	
###*****************************


###*****************************
# read the list to find file name
timeStampFile<-read.csv(file = paste0("../b_results/","parametersCombined",".csv")) #import file
timeStampFile %>%
  dplyr::filter(pick_data=="protein") %>%
  dplyr::filter(growthPhase_names=="ExpAllPhase") %>%
  dplyr::filter(numRepeatsFor_TestTrainSubset_Choice==60) %>%
  dplyr::filter(ndivision==25) %>%
  dplyr::filter(testConditions==paste0(c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase"),collapse = "_"))->chosenDataSetInfo


chosenDataSetInfo[1,]->chosenDataSetInfo

if(nrow(chosenDataSetInfo)!=1){stop("one than one file selected")}

fileName=as.vector(chosenDataSetInfo$fileName)
load(file = paste0("../b_results/",fileName,".RDA"))
###*****************************


###*****************************
testConditions=c("Na_mM_Levels","Mg_mM_Levels","carbonSource","growthPhase")
###*****************************


#******************************************
# Reshape the results of parallel processing (parallel_Result)
result_ListSVM<-NULL
performanceDfSVM<-NULL
result_ListRF<-NULL
performanceDfRF<-NULL

for (counter01 in 1:timeStampVector$numRepeatsFor_TestTrainSubset_Choice)
{
  if(counter01==1)
  {
    result_ListSVM=parallel_Result[[counter01]]$resultListSVM
    performanceDfSVM=parallel_Result[[counter01]]$performanceDfSVM
    result_ListRF=parallel_Result[[counter01]]$resultListRF
    performanceDfRF=parallel_Result[[counter01]]$performanceDfRF
  }
  if(counter01!=1)
  {
    result_ListSVM=dplyr::bind_rows(result_ListSVM,
                                    parallel_Result[[counter01]]$resultListSVM)
    performanceDfSVM=dplyr::bind_rows(performanceDfSVM,
                                      parallel_Result[[counter01]]$performanceDfSVM)
    result_ListRF=dplyr::bind_rows(result_ListSVM,
                                   parallel_Result[[counter01]]$resultListRF)
    performanceDfRF=dplyr::bind_rows(performanceDfRF,
                                     parallel_Result[[counter01]]$performanceDfRF)
  }
}


###*****************************
# Generate the axis names for the figure at the end
axisNames=levels(result_ListSVM$conditionInvestigated) 
###*****************************


###*****************************
result_List<-dplyr::bind_rows(result_ListSVM,result_ListRF)
result_List %>%
  dplyr::mutate(model=ifelse(!is.na(kernel),kernel,"RF"))->result_List

performanceDf<-dplyr::bind_rows(performanceDfSVM,performanceDfRF)
performanceDf$kernel<-as.character(performanceDf$kernel)
performanceDf %>%
  dplyr::mutate(model=ifelse(!is.na(kernel),kernel,"RF"))->performanceDf 
###*****************************


###*****************************
# FIND WINNING MODEL FREQUENCIES (i.e out of 60 runs which run wins how many times)
# not result list only includes the winning model for SVM and the RF. 
# I.e it does not include the best "linear", best "sigmoidal" and best "radial" model. 
result_List %>% 
  dplyr::group_by(TestTrainSubsetNo)%>%
  dplyr::filter(performance==max(performance))%>%
  dplyr::group_by(TestTrainSubsetNo,gamma,cost,nodesize,mtry,ntree)%>%
  dplyr::summarise(model=unique(model), performance=unique(performance))%>%
  dplyr::group_by(model)%>%
  dplyr::summarise(number=n())->modelFreq
###*****************************


###*****************************
# Model performance distributions
performanceDf %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error)%>%
  dplyr::group_by(model, runNum, gamma, cost, nodesize, mtry, ntree) %>%
  dplyr::summarize(error=unique(error), dispersion=unique(dispersion), 
                   performance=unique(performance))%>%
  dplyr::group_by(model, runNum) %>%
  dplyr::select(error, performance) %>%
  dplyr::filter(performance==max(performance))%>%
  unique(.)%>%
  dplyr::summarize(performance=unique(performance), 
                   error=unique(error)) %>%
  dplyr::group_by(model)%>%
  dplyr::mutate(meanPerformance=mean(performance))%>%
  dplyr::mutate(experiment="protein")->winnerModelsForEachExp


fig01<-ggplot(winnerModelsForEachExp, aes(x=model, y=performance, group=model))+
  geom_violin(fill="grey80")+
  geom_point(aes(x=model, y=meanPerformance))

print(fig01)

write.csv(x = winnerModelsForEachExp, file = "../b_results/model_performance_protein.csv")
###*****************************


###*****************************
# Parameter Histograms 2D
# linear
performanceDf  %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error,
                logCost=log10(cost),
                logGamma=log10(gamma))%>%
  dplyr::filter(model=="linear") %>%
  dplyr::select(-gamma, -dispersion)%>%
  dplyr::group_by(cost)%>%
  dplyr::mutate(meanPerformance=mean(performance))%>%
  unique(.) ->linear_performance_cost

fig02a<-ggplot(linear_performance_cost, aes(x=logCost, y=performance, group=logCost))+
  geom_violin(fill="grey80")+
  geom_point(aes(y=meanPerformance))

print(fig02a)


# radial
performanceDf  %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error,
                logCost=log10(cost),
                logGamma=log10(gamma))%>%
  dplyr::filter(model=="radial") %>%
  dplyr::select(-dispersion)%>%
  dplyr::group_by(cost, gamma)%>%
  dplyr::summarize(meanPerformance=mean(performance), meanError=mean(error),
                   logCost=unique(logCost), logGamma=unique(logGamma))%>%
  unique(.) ->radial_performance_cost_gamma

radial_performance_cost_gamma%>%
  dplyr::group_by()%>%
  dplyr::filter(meanPerformance==max(meanPerformance))->max_radial_performance_cost_gamma

fig02b<-ggplot(radial_performance_cost_gamma, aes(x=logCost, y=logGamma, z=meanPerformance))+
  geom_tile(aes(fill=meanPerformance), colour = "grey50")+
  scale_fill_continuous(name="Perf.")+
  geom_point(data = max_radial_performance_cost_gamma, aes(x=logCost, y=logGamma), colour="red")


print(fig02b)

# Sigmoidal
performanceDf  %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error,
                logCost=log10(cost),
                logGamma=log10(gamma))%>%
  dplyr::filter(model=="sigmoid") %>%
  dplyr::select(-dispersion)%>%
  dplyr::group_by(cost, gamma)%>%
  dplyr::summarize(meanPerformance=mean(performance), meanError=mean(error),
                   logCost=unique(logCost), logGamma=unique(logGamma))%>%
  unique(.) ->sigmoid_performance_cost_gamma

sigmoid_performance_cost_gamma%>%
  dplyr::group_by()%>%
  dplyr::filter(meanPerformance==max(meanPerformance))->max_sigmoid_performance_cost_gamma

fig02c<-ggplot(sigmoid_performance_cost_gamma, aes(x=logCost, y=logGamma, z=meanPerformance))+
  geom_tile(aes(fill=meanPerformance), colour = "grey50")+
  scale_fill_continuous(name="Perf.")+
  geom_point(data = max_sigmoid_performance_cost_gamma, aes(x=logCost, y=logGamma), colour="red")

print(fig02c)


# RF
performanceDf  %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error,
                logCost=log10(cost),
                logGamma=log10(gamma))%>%
  dplyr::filter(model=="RF") %>%
  dplyr::select(-dispersion) %>%
  dplyr::group_by(nodesize, mtry, ntree)%>%
  dplyr::summarize(meanPerformance=mean(performance), meanError=mean(error))%>%
  unique(.) %>%
  dplyr::group_by()%>%
  dplyr::mutate(maximum=factor(ifelse(meanPerformance==max(meanPerformance),1,0)))->RF_performance_nodsize_mtry_ntree

RF_performance_nodsize_mtry_ntree%>%
  dplyr::group_by()%>%
  dplyr::filter(meanPerformance==max(meanPerformance))->max_RF_performance_nodsize_mtry_ntree

fig02d<-ggplot(RF_performance_nodsize_mtry_ntree, aes(x=nodesize, y=mtry, z=meanPerformance))+
  facet_grid(.~ntree)+
  geom_tile(aes(fill=meanPerformance), colour = "grey50")+
  scale_fill_continuous(name="Perf.")+
  geom_point(aes(colour="red", alpha=maximum))+
  scale_alpha_discrete(range = c(0,1), guide=FALSE)+
  scale_color_discrete(guide=FALSE)

print(fig02d)


# Combine
temp01<-cowplot::plot_grid(fig02a,fig02b,fig02c,ncol = 3, nrow = 1, labels = c("A","B","C"), scale = .9)
combinedParameters<-cowplot::plot_grid(temp01,fig02d,ncol = 1, nrow = 2, labels = c("", "D"), scale= .9)
print(combinedParameters)

cowplot::save_plot(filename = "../b_figures/combinedParameters_protein.jpeg", plot = combinedParameters, ncol = 3, nrow = 2)
###*****************************


###*****************************
# Calculate Model Frequency
winnerModelsForEachExp %>%
  dplyr::group_by(runNum) %>%
  dplyr::filter(performance==max(performance))%>%
  dplyr::group_by(model) %>%
  dplyr::summarise(numWin=n())->modelFrequency

modelFrequency %>%
  dplyr::filter(numWin==max(numWin)) %>%
  .$model->chosenModel
###*****************************


###*****************************
result_List  %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(logCost=log10(cost),
                logGamma=log10(gamma))%>%
  dplyr::filter(model==chosenModel) %>%
  dplyr::group_by(TestTrainSubsetNo)%>%
  dplyr::filter(performance==max(performance))->result_List


# Calculating Percentages for predictions and generate summary df.
result_List %>%
  dplyr::group_by(conditionInvestigated) %>%
  dplyr::summarise(conditionLength=length(conditionInvestigated)) %>%
  dplyr::left_join(result_List, .)->result_List


result_List %>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)->result_ListSum
###*****************************####


###*****************************
#Prepeare data frame for figure
result_ListSum %>%
  dplyr::group_by()%>%
  tidyr::complete(predictedValue=axisNames,conditionInvestigated=axisNames)->result_ListSum
result_ListSum[is.na(result_ListSum)] <- 0


result_ListSum$conditionInvestigated<-factor(result_ListSum$conditionInvestigated)
result_ListSum$predictedValue<-factor(result_ListSum$predictedValue)
result_ListSum$predictedValue <- factor(result_ListSum$predictedValue, 
                                        levels=rev(levels(result_ListSum$predictedValue)))
###*****************************


###*****************************
# Make the order of labels correct in "result_ListSum" DF which includes a list of predictions
result_ListSum$conditionInvestigated <- factor(result_ListSum$conditionInvestigated, 
                                               levels=rev(factorOrder))
result_ListSum$predictedValue <- factor(result_ListSum$predictedValue, 
                                        levels=(factorOrder))
###*****************************


###*****************************
# Genrate a tidy DF that combines "Names", "Short_names" "colors", and order in label table
expandedGrid %>%
  dplyr::filter(conditionInvestigatedVector %in% factorOrder)%>%
  dplyr::select(-conditionInvestigatedVector)%>%
  dplyr::mutate(axis1=1:n(),
                axis2=n():1)%>%
  tidyr::gather(key=condition,value = variable,1:length(testConditions))->tidyDF_A

expandedGrid_Short %>%
  dplyr::filter(conditionInvestigatedVector_Short %in% factorOrder_Short)%>%
  dplyr::select(-conditionInvestigatedVector_Short)%>%
  dplyr::mutate(axis1=1:n(),
                axis2=n():1)%>%
  tidyr::gather(key=condition_Short,value = variable_Short,1:length(testConditions))%>%
  dplyr::mutate(condition=condition_Short)%>%
  dplyr::select(-condition_Short)->tidyDF_B
tidyDF_B$condition=gsub("_Short","",tidyDF_B$condition)

tidyDF<-left_join(tidyDF_A,tidyDF_B)

# Order the labels of tidyDF
tidyDF$condition <- factor(tidyDF$condition,levels=(testConditions))

valuesOrdered=as.vector(rle(unlist((vectorList[testConditions])))$values)
tidyDF$variable <- factor(tidyDF$variable,levels=valuesOrdered)

valuesOrdered_Short=as.vector(rle(unlist((vectorList_Short[paste0(testConditions,"_Short")])))$values)
tidyDF$variable_Short <- factor(tidyDF$variable_Short,levels=valuesOrdered_Short)

# Merge colors to tidyDF
tidyDF%>%
  dplyr::mutate(colorCodes=ifelse(variable=="Glucose","#6a51a3",NA),
                colorCodes=ifelse(variable=="Gluconate","#9e9ac8",colorCodes),
                colorCodes=ifelse(variable=="Lactate","#bcbddc",colorCodes),
                colorCodes=ifelse(variable=="Glycerol","#807dba",colorCodes),
                colorCodes=ifelse(variable=="Exponential","#bae4b3",colorCodes),
                colorCodes=ifelse(variable=="Stationary","#74c476",colorCodes),
                colorCodes=ifelse(variable=="Late-Stationary","#238b45",colorCodes),
                colorCodes=ifelse(variable=="Base Na","#fdbe85",colorCodes),
                colorCodes=ifelse(variable=="High Na","#fd8d3c",colorCodes),
                colorCodes=ifelse(variable=="Low Mg","#bdd7e7",colorCodes),
                colorCodes=ifelse(variable=="Base Mg","#6baed6",colorCodes),
                colorCodes=ifelse(variable=="High Mg","#2171b5",colorCodes))->tidyDF

colorCodes<-as.vector(tidyDF$colorCodes)
names(colorCodes)<-as.vector(tidyDF$variable)
###*****************************


###*****************************
# Generate Associated Figures
fig03<-ggplot(result_ListSum, aes( y=conditionInvestigated,x= predictedValue))+
  geom_tile(aes(fill=percentPrediction),colour = "grey50")+
  scale_fill_gradient(low = "White", high = "Black",limits=c(0,100),name = "% Prediction")+
  geom_text(aes(label=sprintf("%1.0f", percentPrediction)),size=8, color="white")+
  scale_x_discrete(expand=c(0,0), breaks=1:length(levels(result_ListSum$conditionInvestigated))) +   # Set x-breaks here
  coord_cartesian(xlim=c(0.5, length(levels(result_ListSum$conditionInvestigated))+0.5))+
  scale_y_discrete(expand=c(0,0), breaks=1:length(levels(result_ListSum$conditionInvestigated))) +   # Set x-breaks here
  coord_cartesian(ylim=c(0.5, length(levels(result_ListSum$conditionInvestigated))+0.5))+
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x=element_text(size=0),
         axis.text.y=element_blank(),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14),
         legend.position="none")

#fig03<-ggdraw(switch_axis_position(fig03, axis = 'x'))

print(fig03)

fig04<-ggplot(result_ListSum, aes( y=conditionInvestigated,x= predictedValue))+
  geom_tile(aes(fill=percentPrediction))+
  scale_fill_gradient(low = "White", high = "Black",limits=c(0,100),name = "% Prediction")+
  geom_text(aes(label=sprintf("%1.0f", percentPrediction)),size=8)+
  theme_bw()+
  #xlab("predicted value")+
  #ylab("true value")+
  #ggtitle("mRNA PCoA D12 AllPhase")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x=element_text(size=16,angle = 90, hjust = 1),
         axis.text.y=element_text(size=16),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14))

percentLegendObj <- gtable_filter(ggplotGrob(fig04), "guide-box")
figPercent=ggdraw(percentLegendObj)
print(figPercent)


fig05<-ggplot(tidyDF, aes( x=axis1, y=condition))+
  
  geom_tile(aes(fill=variable))+
  scale_fill_manual(values=colorCodes)+ # Color bar
  geom_text(aes(label=variable_Short),fontface="bold") + 
  scale_x_discrete(expand=c(0,0), breaks=1:max(tidyDF$axis1)) +   # Set x-breaks here
  coord_cartesian(xlim=c(0.5, max(tidyDF$axis1)+0.5)) + # To get rid of white space  
  guides(fill = guide_legend(title="Condition", 
                             nrow=ceiling(length(valuesOrdered)/6),
                             byrow = TRUE))+
  theme(legend.position="bottom",
        legend.key.size= unit(.6,"cm"))
print(fig05)

variableLegendObj <- gtable_filter(ggplotGrob(fig05), "guide-box")
figPercent=ggdraw(variableLegendObj)
print(variableLegendObj)


tidyDF$condition <- factor(tidyDF$condition,levels=rev(testConditions))

fig06<-ggplot(tidyDF, aes( y=axis2, x=condition))+
  geom_tile(aes(fill=variable))+
  scale_fill_manual(values=colorCodes)+ # Color bar
  geom_text(aes(label=variable_Short),fontface="bold") + 
  scale_y_discrete(expand=c(0,0), breaks=1:max(tidyDF$axis1)) +   # Set x-breaks here
  coord_cartesian(ylim=c(0.5, max(tidyDF$axis1)+0.5))+   # To get rid of white space  
  guides(fill = guide_legend(title="Condition", 
                             nrow=ceiling(length(valuesOrdered)/6),
                             byrow = TRUE))+
  theme(legend.position="bottom",
        legend.key.size= unit(.6,"cm"))
print(fig06)



# Combine Figures
# Rename figure 1
figComb=fig03

#take parts from figure3
color_x <- gtable_filter(ggplotGrob(fig06), "panel")
color_y <- gtable_filter(ggplotGrob(fig05), "panel")
color_legend <- gtable_filter(ggplotGrob(fig05), "guide-box")
# color_x_categories <- gtable_filter(ggplotGrob(fig02a), "axis-l")

# Add the new x axis
g.main <- ggplotGrob(figComb)
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.15, "null")), index$t-1)
g.main <- gtable_add_grob(g.main, color_y,
                          t = index$b,
                          l = index$l,
                          b = index$b,
                          r = index$r,
                          name="color_y")

# Add the new y axis
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(.25, "null")), index$l-1)
g.main <- gtable_add_grob(g.main, color_x,
                          t = index$b,
                          l = index$l,
                          b = index$b,
                          r = index$r,
                          name="color_x")

figComb=ggdraw(g.main)
print(figComb)


# Add the color legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_rows(g.main, unit.c(unit(.08, "null")), index$t+3)
g.main <- gtable_add_grob(g.main, color_legend,
                          t = index$b+4,
                          l = index$l,
                          b = index$b+4,
                          r = index$r,
                          name="color_legend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "color_legend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)

# add percent legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(.17, "null")), index$l+3)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+4,
                          b = index$b,
                          r = index$r+4,
                          name="percentLegendObj")

figComb_wL=ggdraw(g.main)
print(figComb_wL)

color_legend=ggdraw(color_legend)
print(color_legend)
###*****************************


#doNotSave=1
if(! exists("doNotSave"))
{
  ###*****************************
  # Save Figure
  cowplot::save_plot(figComb,
                     filename = paste0("../b_figures/fig_protein.pdf"),
                     base_aspect_ratio=1.333, base_height = 6, 
                     units = "in", useDingbats=FALSE, limitsize=FALSE,
                     ncol = 1.4, nrow=1.2)
  
  
  # Save Figure with legend
  cowplot::save_plot(figComb_wL,
                     filename = paste0("../b_figures/fig_withLegend_protein.pdf"),
                     base_aspect_ratio=1.333, base_height = 6, 
                     units = "in", useDingbats=FALSE, limitsize=FALSE,
                     ncol = 1.4, nrow=1.2)
  
  # Save Figure Object
  fileNameCollapsed=paste(fileName,collapse = "_")
  save(list = c("figComb","figComb_wL","percentLegendObj","color_legend"),
       file = paste0("../b_figures/fig_obj_protein.Rda"))
  
  ##Save Full Color Legend
  save(list = c("color_legend"),
       file = paste0("../b_figures/","fullColorLegend.Rda"))
  ###*****************************
}


###*****************************
result_ListSum%>%
  dplyr::filter(predictedValue==conditionInvestigated)%>%
  .$percentPrediction %>%sum(.)->success

print(success)
###*****************************