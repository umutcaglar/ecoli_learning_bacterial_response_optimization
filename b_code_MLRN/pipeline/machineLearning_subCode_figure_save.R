###*****************************
# Generate the axis names for the figure at the end
axisNames=levels(inputMetaDf$conditionInvestigated) 
###*****************************


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
fig01<-ggplot(result_ListSum, aes( y=conditionInvestigated,x= predictedValue))+
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
         axis.title.x = element_blank(),
         axis.title.y=element_blank(),
         axis.text.x=element_text(size=0),
         axis.text.y=element_blank(),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14),
         legend.position="none")

#fig01<-ggdraw(switch_axis_position(fig01, axis = 'x'))

print(fig01)

fig02<-ggplot(result_ListSum, aes( y=conditionInvestigated,x= predictedValue))+
  geom_tile(aes(fill=percentPrediction))+
  scale_fill_gradient(low = "White", high = "Black",limits=c(0,100),name = "% Prediction")+
  geom_text(aes(label=sprintf("%1.0f", percentPrediction)),size=8)+
  theme_bw()+
  #xlab("predicted value")+
  #ylab("true value")+
  #ggtitle("mRNA PCoA D12 AllPhase")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.title.x = element_text(size=0),
         axis.title.y=element_blank(),
         axis.text.x=element_text(size=16,angle = 90, hjust = 1),
         axis.text.y=element_text(size=16),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14))

percentLegendObj <- gtable_filter(ggplotGrob(fig02), "guide-box")
figPercent=ggdraw(percentLegendObj)
print(figPercent)


fig03<-ggplot(tidyDF, aes( x=axis1, y=condition))+
  
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
print(fig03)

variableLegendObj <- gtable_filter(ggplotGrob(fig03), "guide-box")
figPercent=ggdraw(variableLegendObj)
print(variableLegendObj)


tidyDF$condition <- factor(tidyDF$condition,levels=rev(testConditions))

fig04<-ggplot(tidyDF, aes( y=axis2, x=condition))+
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
print(fig04)



# Combine Figures
# Rename figure 1
figComb=fig01

#take parts from figure3
color_x <- gtable_filter(ggplotGrob(fig04), "panel")
color_y <- gtable_filter(ggplotGrob(fig03), "panel")
color_legend <- gtable_filter(ggplotGrob(fig03), "guide-box")
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
g.main <- gtable_add_rows(g.main, unit.c(unit(.05, "null")), index$t+2)
g.main <- gtable_add_grob(g.main, color_legend,
                          t = index$b+2,
                          l = index$l,
                          b = index$b+2,
                          r = index$r,
                          name="color_legend")

#add space above color legend
#add a row as spacer 
index <- subset(g.main$layout, name == "color_legend")
g.main <- gtable_add_rows(g.main, unit.c(unit(0.35, "in")), index$b-2)

# add percent legend
index <- subset(g.main$layout, name == "panel")
g.main <- gtable_add_cols(g.main, unit.c(unit(.17, "null")), index$l+2)
g.main <- gtable_add_grob(g.main, percentLegendObj,
                          t = index$b,
                          l = index$l+2,
                          b = index$b,
                          r = index$r+2,
                          name="percentLegendObj")

figComb_wL=ggdraw(g.main)
print(figComb_wL)

color_legend=ggdraw(color_legend)
print(color_legend)
###*****************************


###*****************************
# Generate file name based on date
options(digits.secs=6)
time1<-Sys.time(); 
print(time1); 
time2<-gsub(pattern = "*.*\\.",replacement = "", x = time1);
time3<-paste0(format(time1, "%Y_%m_%d_%H_%M_%S"),"_",time2)
fileName=paste0("No_",time3)
###*****************************


###*****************************
# generating meta object
dataInfo<-dataNameDF
colnames(dataInfo)=gsub(pattern = "objectName.",replacement = "",x = colnames(dataInfo))

dataInfo$initial=fileName
dataInfo$numRepeatsFor_TestTrainSubset_Choice=numRepeatsFor_TestTrainSubset_Choice
dataInfo$mapping=dimReductionType
dataInfo$dimensions=paste0("D",dimensionChoice)
dataInfo$type_svm=type_svmChoice
dataInfo$kernel_type=kernel_typeChoice
dataInfo$cost = modelSVM$cost
dataInfo$gamma = modelSVM$gamma
if(length(testConditions)==1){dataInfo$testFor=testConditions}
if(length(testConditions)!=1){dataInfo$testFor=paste(testConditions,collapse = "_")}

if(length(similarDataClassifierForBatch)==1){dataInfo$similarDataClassifierForBatch=similarDataClassifierForBatch}
if(length(similarDataClassifierForBatch)!=1){dataInfo$similarDataClassifierForBatch=paste(similarDataClassifierForBatch,collapse = "_")}

dataInfo$batchCorrectionMethod=batchCorrectionMethod
dataInfo$batchCorrectionType=batchCorrectionType
dataInfo$classWeightInputType=classWeightInputType

dataInfo$tuning = tuning
dataInfo$parallel = parallel_com
dataInfo$numCore = numCore

result_ListSum %>% 
  dplyr::filter(predictedValue==conditionInvestigated) %>%
  .$percentPrediction %>%
  sum(.)->trace

dataInfo$trace<-trace

result_ListSum %>% 
  dplyr::filter(predictedValue==conditionInvestigated)%>%
  nrow(.)->numDistictConditions

score<-(trace-100)/((numDistictConditions-1)*100)

dataInfo$score<-score
###*****************************


if(! exists("doNotSave"))
{
  ###*****************************
  # Save Result Data Frame
  write.csv(x = result_List, file=paste0("../b_results_v2/","outputDF_",fileName,".csv"))
  ###*****************************
  
  
  ###*****************************
  # Results summary file 
  
  # a individual result summary file
  parameters=data.frame(parameters = t(dataInfo))
  write.csv(x = parameters,file=paste0("../b_results_v2/","parameters_",fileName,".csv"),row.names = TRUE)
  
  # b combined result summary file
  parametersCombined<-read.csv(file=paste0("../b_results_v2/","parametersCombined",".csv"),header = TRUE,row.names = )
  
  dplyr::bind_rows(parametersCombined,dataInfo) %>%
    unique(.)->parametersCombined
  
  write.csv(x=parametersCombined, file=paste0("../b_results_v2/","parametersCombined",".csv"),row.names = FALSE)
  ###*****************************
  
  
  ###*****************************
  # Save Figure
  cowplot::save_plot(figComb,
                     filename = paste0("../b_figures_v2/fig_",fileName,".pdf"),
                     base_aspect_ratio=1.333, base_height = 6, 
                     units = "in", useDingbats=FALSE, limitsize=FALSE,
                     ncol = 1.4, nrow=1.2)
  
  
  # Save Figure with legend
  cowplot::save_plot(figComb_wL,
                     filename = paste0("../b_figures_v2/fig_withLegend_",fileName,".pdf"),
                     base_aspect_ratio=1.333, base_height = 6, 
                     units = "in", useDingbats=FALSE, limitsize=FALSE,
                     ncol = 1.4, nrow=1.2)
  
  # Save Figure Object
  fileNameCollapsed=paste(fileName,collapse = "_")
  save(list = c("figComb","figComb_wL","percentLegendObj","color_legend"),
       file = paste0("../b_results_v2/fig_obj_",fileName,".Rda"))
  
  ##Save Full Color Legend
  save(list = c("color_legend"),
       file = paste0("../b_results_v2/","fullColorLegend.Rda"))
  ###*****************************
}


