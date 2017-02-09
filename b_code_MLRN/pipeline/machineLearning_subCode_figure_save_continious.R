###*****************************
# Generate the axis names for the figure at the end
axisNames=levels(inputMetaDf$conditionInvestigated) 
###*****************************


###*****************************
# figure scales 
if(testConditions=="Na_mM"){lowLimit=0; upLimit=9.5}
if(testConditions=="Mg_mM"){lowLimit=-9.5; upLimit=9.5}
if(testConditions=="growthTime_hr"){lowLimit=0; upLimit=9}
###*****************************


###*****************************
fig01<-ggplot(result_List, aes( x=conditionInvestigated,y= predictedValue))+
  #geom_boxplot(aes(group = cut_width(conditionInvestigated, 0.10)))+
  #geom_violin(aes(group = conditionInvestigated),scale = "width")+
  geom_point()+
  geom_abline(intercept = 0, slope = 1,color="red")+
  stat_summary(fun.y=mean, colour="orange", geom="point", shape=18, size=3,show.legend = FALSE)+
  theme_bw()+
  #expand_limits(x =0, y = 0)+
  theme( panel.grid.major = element_blank(),
         #panel.grid.minor = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x=element_text(size=16),
         axis.text.y=element_text(size=16),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14))

axisTicks=ggplot_build(fig01)$panel$ranges[[1]]$x.major_source
axisLabels=sprintf("%.0f",2^ggplot_build(fig01)$panel$ranges[[1]]$x.major_source)

fig01<-ggplot(result_List, aes( x=log2(conditionInvestigated),y= log2(predictedValue)))+
  geom_boxplot(aes(group = cut_width(conditionInvestigated, 0.05)))+
  #geom_violin(aes(group = conditionInvestigated),scale = "width")+
  #geom_point()+
  #geom_line(aes(group=factor(TestTrainSubsetNo), color=factor(TestTrainSubsetNo)))+
  geom_abline(intercept = 0, slope = 1,color="red")+
  stat_summary(fun.y=mean, colour="orange", geom="point", shape=18, size=3,show.legend = FALSE)+
  theme_bw()+
  scale_x_continuous(limits = c(lowLimit,upLimit),breaks =axisTicks ,labels = axisLabels)+
  scale_y_continuous(limits = c(lowLimit,upLimit),breaks =axisTicks ,labels = axisLabels)+
  theme( panel.grid.major = element_blank(),
         #panel.grid.minor = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x=element_text(size=16),
         axis.text.y=element_text(size=16),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14))

print(fig01)

fit.lm = lm(formula = conditionInvestigated ~ predictedValue, data = result_List)
summary(fit.lm)$r.squared 

###*****************************
# Generate associated figure

###*****************************







###*****************************
# Generate associated figure
fig01<-ggplot(result_List, aes( x=log(conditionInvestigated),y= log(predictedValue)))+
  geom_abline(intercept = 0, slope = 1,color="#d9d9d9",linetype=2)+
  geom_boxplot(aes(group = cut_width(conditionInvestigated, 0.05)),
               outlier.colour=NA,outlier.size = 0.3)+
  #geom_violin(aes(group = conditionInvestigated),scale = "width")+
  stat_summary(fun.y=mean, colour="#f16913", geom="point", shape=95, size=9,show.legend = FALSE)+
  theme_bw()+
  expand_limits(x =0, y = 0)+
  ylim(lowLimit,upLimit)+
  scale_x_continuous(limits = c(lowLimit,upLimit))+
  theme( panel.grid.major = element_blank(),
         #panel.grid.minor = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x=element_text(size=16),
         axis.text.y=element_text(size=16),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14))

#print(fig01)

axisTicks=ggplot_build(fig01)$panel$ranges[[1]]$x.major_source
axisLabels=sprintf("%.0f",2^ggplot_build(fig01)$panel$ranges[[1]]$x.major_source)

fig01<-ggplot(result_List, aes( x=log(conditionInvestigated),y= log(predictedValue)))+
  geom_abline(intercept = 0, slope = 1,color="#7570b3",linetype=2, size=1.2)+
  geom_boxplot(aes(group = cut_width(conditionInvestigated, 1)),
               outlier.colour=NA,outlier.size = 0.3)+
  #geom_violin(aes(group = conditionInvestigated),scale = "width")+
  stat_summary(fun.y=mean, colour="#f16913", geom="point", shape=95, size=9,show.legend = FALSE)+
  theme_bw()+
  expand_limits(x =0, y = 0)+
  ylim(lowLimit,upLimit)+
  scale_x_continuous(limits = c(lowLimit,upLimit),breaks =axisTicks ,labels = axisLabels)+
  scale_y_continuous(limits = c(lowLimit,upLimit),breaks =axisTicks ,labels = axisLabels)+
  theme( panel.grid.major = element_blank(),
         #panel.grid.minor = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x=element_text(size=16),
         axis.text.y=element_text(size=16),
         axis.title.x=element_text(size=16),
         axis.title.y=element_text(size=16),
         legend.title=element_text(size=14),
         legend.text=element_text(size=14),
         strip.text = element_text(size = 14))

print(fig01)
###*****************************










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


