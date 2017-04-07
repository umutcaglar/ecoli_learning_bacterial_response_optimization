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
    result_List_linear=parallel_Result[[counter01]]$resultListSVM_linear
    result_List_radial=parallel_Result[[counter01]]$resultListSVM_radial
    result_List_sigmoid=parallel_Result[[counter01]]$resultListSVM_sigmoid
    result_List_RF=parallel_Result[[counter01]]$resultListRF

    performanceDf_linear=parallel_Result[[counter01]]$performanceDf_linear
    performanceDf_linear$runNum=1
    performanceDf_radial=parallel_Result[[counter01]]$performanceDf_radial
    performanceDf_radial$runNum=1
    performanceDf_sigmoid=parallel_Result[[counter01]]$performanceDf_sigmoid
    performanceDf_sigmoid$runNum=1
    performanceDf_RF=parallel_Result[[counter01]]$performanceDfRF
    performanceDf_RF$runNum=1
  }

  if(counter01!=1)
  {
    result_List_linear=dplyr::bind_rows(result_List_linear, parallel_Result[[counter01]]$resultListSVM_linear)
    result_List_radial=dplyr::bind_rows(result_List_radial, parallel_Result[[counter01]]$resultListSVM_radial)
    result_List_sigmoid=dplyr::bind_rows(result_List_sigmoid, parallel_Result[[counter01]]$resultListSVM_sigmoid)
    result_List_RF=dplyr::bind_rows(result_List_RF,parallel_Result[[counter01]]$resultListRF)

    performanceDf_linear_temp = parallel_Result[[counter01]]$performanceDf_linear
    performanceDf_linear_temp$runNum=counter01
    performanceDf_linear=dplyr::bind_rows(performanceDf_linear, performanceDf_linear_temp)

    performanceDf_radial_temp = parallel_Result[[counter01]]$performanceDf_radial
    performanceDf_radial_temp$runNum=counter01
    performanceDf_radial=dplyr::bind_rows(performanceDf_radial, performanceDf_radial_temp)

    performanceDf_sigmoid_temp = parallel_Result[[counter01]]$performanceDf_sigmoid
    performanceDf_sigmoid_temp$runNum=counter01
    performanceDf_sigmoid=dplyr::bind_rows(performanceDf_sigmoid, performanceDf_sigmoid_temp)

    performanceDf_RF_temp = parallel_Result[[counter01]]$performanceDfRF
    performanceDf_RF_temp$runNum=counter01
    performanceDf_RF=dplyr::bind_rows(performanceDf_RF, performanceDf_RF_temp)
  }
}


###*****************************
# Generate the axis names for the figure at the end
axisNames=levels(result_List_radial$conditionInvestigated)
###*****************************


###*****************************
# Combine Results and organize DF's

# a) Result list (best of 60 runs including predictions)
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

# b) Performance data frame performance of all distinct runs
performanceDf_linear$model="linear"
performanceDf_radial$model="radial"
performanceDf_sigmoid$model="sigmoid"
performanceDf_RF$model="RF"

performanceDf<-dplyr::bind_rows(performanceDf_linear,
                                performanceDf_radial,
                                performanceDf_sigmoid,
                                performanceDf_RF)

performanceDf %>%
  dplyr::select(runNum,model,
                cost=costList,gamma=gammaList,
                nodesize, mtry, ntree,
                error=error.val,tryList, tryListNo) %>%
  dplyr::group_by(runNum, model,
                  cost, gamma, nodesize, mtry, ntree) %>%
  dplyr::summarize(error=mean(error), dispersion=sd(error))->performanceDf

performanceDf  %>%
  dplyr::mutate(performance=1-error,
                logCost=log10(cost),
                logGamma=log10(gamma))%>%
  dplyr::select(runNum, model,
                cost, logCost, gamma, logGamma, nodesize, mtry, ntree,
                error, performance, dispersion)->performanceDf
###*****************************



###*****************************
# Find the 240 =60 x 4 best models
result_List %>%
  dplyr::group_by(TestTrainSubsetNo, model) %>%
  dplyr::mutate(error_test=F1ScoreErrCpp(predictedValue, conditionInvestigated))%>%
  dplyr::summarize(performance=unique(performance),
                   cost = unique(cost),
                   gamma = unique(gamma),
                   nodesize = unique(nodesize),
                   mtry = unique(mtry),
                   ntree = unique(ntree),
                   error_test=unique(error_test))%>%
  dplyr::mutate(performance_test=1-error_test)%>%
  dplyr::arrange(model, TestTrainSubsetNo)->result_List_sum  # the file that have length of 60x4

result_List_sum  %>%
  dplyr::group_by(model)%>%
  dplyr::mutate(meanPerformance=mean(performance),
                meanPerformance_test=mean(performance_test)) %>%  # calculate mean performance of each model
  dplyr::mutate(analyzeName=analyzeName)->result_List_sum  # add the analyze name

fig01<-ggplot(result_List_sum, aes(x=model, y=performance, group=model))+
  geom_violin(fill="grey80")+
  geom_point(aes(x=model, y=meanPerformance))+
  ylim(0.5,1)

print(fig01)

write.csv(x = result_List_sum, file = paste0("../b_results/","model_performance_",analyzeName,".csv"))

result_List_sum%>%
  dplyr::group_by(TestTrainSubsetNo)%>%
  dplyr::filter(performance==max(performance))%>%
  dplyr::group_by(model)%>%
  dplyr::summarise(numWin=n())->modelFreq  # number of wins for each model for different Test-Train subsets

modelFreq %>%
  #dplyr::filter(!model=="RF") %>% # We need to solve this
  dplyr::filter(numWin==max(numWin)) %>%
  .$model->chosenModel # chosen model based on most wins

###*****************************




###*****************************
# Parameter Histograms 2D
# linear
performanceDf  %>%
  dplyr::filter(model=="linear") %>%
  dplyr::group_by(cost)%>%
  dplyr::mutate(meanPerformance=mean(performance))%>%
  unique(.) ->linear_performance_cost

fig02a<-ggplot(linear_performance_cost, aes(x=logCost, y=performance, group=logCost))+
  geom_violin(fill="grey80")+
  geom_point(aes(y=meanPerformance))

print(fig02a)


# radial
performanceDf  %>%
  dplyr::filter(model=="radial") %>%
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
  dplyr::filter(model=="sigmoid") %>%
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
  dplyr::filter(model=="RF") %>%
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


# Combine all 4 plots fig02a, fig02b, fig02c, fig02d
combinedParameters<-cowplot::ggdraw() +
  cowplot::draw_plot(plot=fig02a, x=0, y=.5, width=1/3, height=.5) +
  cowplot::draw_plot(plot=fig02b, x=1/3, y=0.5, width=1/3, height=.5) +
  cowplot::draw_plot(plot=fig02c, x=2/3, y=0.5, width=1/3, height=.5) +
  cowplot::draw_plot(plot=fig02d, x=0, y=0, width=1, height=.5) +
  cowplot::draw_plot_label(c("A", "B", "C", "D"), c(0, 1/3, 2/3, 0), c(1, 1, 1, .5), size = 15)

print(combinedParameters)

cowplot::save_plot(filename = paste0("../b_figures/","combinedParameters_",analyzeName,".jpeg"),
                   plot = combinedParameters, ncol = 3, nrow = 2)
###*****************************



# Generate the Square Figure
###*****************************
###*****************************
result_List  %>%
  dplyr::filter(model==chosenModel) %>%
  dplyr::group_by(TestTrainSubsetNo)%>%
  dplyr::filter(performance==max(performance))->winnerModelResults


# Calculating Percentages for predictions and generate summary df.
winnerModelResults %>%
  dplyr::group_by(conditionInvestigated) %>%
  dplyr::summarise(conditionLength=length(conditionInvestigated)) %>%
  dplyr::left_join(winnerModelResults, .)->winnerModelResults


winnerModelResults %>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)->winnerModelResultsSum

winnerModelResultsSum%>%
  dplyr::filter(predictedValue==conditionInvestigated)%>%
  .$combinationLength %>%sum(.)->diagonalSum_samples
winnerModelResultsSum%>%
  dplyr::filter(predictedValue==conditionInvestigated)%>%
  .$percentPrediction %>%sum(.)->diagonalSum_percents
###*****************************####


###*****************************
#Prepeare data frame for figure
winnerModelResultsSum %>%
  dplyr::group_by()%>%
  tidyr::complete(predictedValue=axisNames,conditionInvestigated=axisNames)->winnerModelResultsSum
winnerModelResultsSum[is.na(winnerModelResultsSum)] <- 0


winnerModelResultsSum$conditionInvestigated<-factor(winnerModelResultsSum$conditionInvestigated)
winnerModelResultsSum$predictedValue<-factor(winnerModelResultsSum$predictedValue)
winnerModelResultsSum$predictedValue <- factor(winnerModelResultsSum$predictedValue,
                                               levels=rev(levels(winnerModelResultsSum$predictedValue)))
###*****************************


###*****************************
# Make the order of labels correct in "winnerModelResultsSum" DF which includes a list of predictions
winnerModelResultsSum$conditionInvestigated <- factor(winnerModelResultsSum$conditionInvestigated,
                                                      levels=rev(factorOrder))
winnerModelResultsSum$predictedValue <- factor(winnerModelResultsSum$predictedValue,
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
n_scale=1/2
CombinationLengthMax_scaled=(max(winnerModelResultsSum$combinationLength)+1)^(n_scale)
breakList=seq(from=1,to=round(CombinationLengthMax_scaled),length.out=6)
labelList=round(((breakList)^(1/n_scale))-1)

fig03<-ggplot(winnerModelResultsSum, aes( y=conditionInvestigated,x= predictedValue))+
  
  #geom_tile(aes(fill=((combinationLength+1)^(n_scale)-1)),colour = "grey50")+ # num sample variant
  #scale_fill_gradient(low = "White", high = "Black",limits=c(0,CombinationLengthMax_scaled), # num sample variant
  #                    name = "% Prediction", breaks=breakList, labels=labelList)+
  # geom_text(aes(label=sprintf("%1.0f", combinationLength)),size=5, color="white")+ # num sample variant
  
  geom_tile(aes(fill=percentPrediction),colour = "grey50")+ # num percent variant
  scale_fill_gradient(low = "White", high = "Black",limits=c(0,100), # num percent variant
                       name = "% Prediction")+
  geom_text(aes(label=sprintf("%1.0f", percentPrediction)),size=5, color="white")+ # num percent variant
  
  scale_x_discrete(expand=c(0,0), breaks=1:length(levels(winnerModelResultsSum$conditionInvestigated))) +   # Set x-breaks here
  coord_cartesian(xlim=c(0.5, length(levels(winnerModelResultsSum$conditionInvestigated))+0.5))+
  scale_y_discrete(expand=c(0,0), breaks=1:length(levels(winnerModelResultsSum$conditionInvestigated))) +   # Set x-breaks here
  coord_cartesian(ylim=c(0.5, length(levels(winnerModelResultsSum$conditionInvestigated))+0.5))+
  theme_bw()+
  xlab("")+
  ylab("")+
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

fig04<-ggplot(winnerModelResultsSum, aes( y=conditionInvestigated,x= predictedValue))+
  
  # geom_tile(aes(fill=((combinationLength+1)^(n_scale))-1))+ # num sample variant
  # scale_fill_gradient(low = "White", high = "Black",limits=c(0,CombinationLengthMax_scaled), # num sample variant
  #                    name = "% Prediction", breaks=breakList, labels=labelList)+
  # geom_text(aes(label=sprintf("%1.0f", combinationLength)),size=5)+ # num sample variant
  
  geom_tile(aes(fill=percentPrediction),colour = "grey50")+ # num percent variant
  scale_fill_gradient(low = "White", high = "Black",limits=c(0,100), # num percent variant
                      name = "% Prediction")+
  geom_text(aes(label=sprintf("%1.0f", percentPrediction)),size=5, color="white")+ # num percent variant
  
  theme_bw()+
  xlab("")+
  ylab("")+
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


###*****************************
if(!doNotSave==1)
{
  ###*****************************
  # Save Figure
  cowplot::save_plot(figComb,
                     filename = paste0("../b_figures/","fig_",analyzeName,".pdf"),
                     base_aspect_ratio=1.333, base_height = 6,
                     units = "in", useDingbats=FALSE, limitsize=FALSE,
                     ncol = 1.4, nrow=1.2)


  # Save Figure with legend
  cowplot::save_plot(figComb_wL,
                     filename = paste0("../b_figures/","fig_withLegend_",analyzeName,".pdf"),
                     base_aspect_ratio=1.333, base_height = 6,
                     units = "in", useDingbats=FALSE, limitsize=FALSE,
                     ncol = 1.4, nrow=1.2)

  # Save Figure Object
  fileNameCollapsed=paste(fileName,collapse = "_")
  save(list = c("figComb","figComb_wL","percentLegendObj","color_legend"),
       file = paste0("../b_figures/","fig_obj_",analyzeName,".Rda"))

  ##Save Full Color Legend
  save(list = c("color_legend"),
       file = paste0("../b_figures/","ColorLegend_",analyzeName,".Rda"))
  ###*****************************
}
###*****************************


###*****************************
# Save the meta parameters for analyze
# a) generate meta vector for individual analyze and but it into data file
modelFreqVec<-as.vector(t(modelFreq[,2])); names(modelFreqVec)<-as.vector(t(modelFreq[,1]))
combF1Score<-
  1-F1ScoreErrCpp(winnerModelResults$predictedValue,winnerModelResults$conditionInvestigated)

chosenDataSetInfo->metaVector
metaVector$diagonalSum_samples=diagonalSum_samples
metaVector$diagonalSum_percents=diagonalSum_percents
metaVector$nDistinctCondition=length(axisNames)
metaVector$combF1Score=combF1Score
metaVector$chosenModel=chosenModel
metaVector$linear = ifelse("linear" %in% names(modelFreqVec),yes = try(modelFreqVec[["linear"]],silent = T),no = 0)
metaVector$radial = ifelse("radial" %in% names(modelFreqVec),yes = try(modelFreqVec[["radial"]],silent = T),no = 0)
metaVector$sigmoid = ifelse("sigmoid" %in% names(modelFreqVec),yes = try(modelFreqVec[["sigmoid"]],silent = T),no = 0)
metaVector$RF = ifelse(test = "RF" %in% names(modelFreqVec),yes = try(modelFreqVec[["RF"]],silent = T),no = 0)
metaVector$analyzeName=analyzeName


# b) Save the conditions to csv file
metaFileOld<-read.csv(file = paste0("../b_results/","parametersAnalyzeMetafile",".csv")) #import old file
metaFileOld %>% dplyr::filter(!analyzeName==get("analyzeName"))->metaFileOld
metaFileNew<-dplyr::bind_rows(metaFileOld,metaVector) # add new line
metaFileNew<-unique(metaFileNew) # remove duplicates
write.csv(x=metaFileNew, file=paste0("../b_results/","parametersAnalyzeMetafile",".csv"),row.names = FALSE)
###*****************************


###*****************************
print("Model Frequencies:")
print(modelFreq)


print(paste0("Chosen model: ",chosenModel))
print(paste0("multi class macro F1 score: ", combF1Score))
print(paste0("Diagonal sum of samples. (Before normalization) :",diagonalSum_samples))
print(paste0("Diagonal sum of percents: ",diagonalSum_percents))
print(paste0("Diagonal sum of percents av.:  %",diagonalSum_percents/(length(factorOrder))))
###*****************************
