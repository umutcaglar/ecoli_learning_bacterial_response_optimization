# SVM_line_tune

###*****************************
# A) TUNE

linearTuneResults<-merge(data.frame(costList=costList), tryList, all=TRUE)
linearTuneResults%>%dplyr::mutate(error.val=NA)->linearTuneResults

counter03=0
for(counter02a in 1: crossValue)
{
  trainDataFrame=trainTuneDFs$listDf[[counter02a]]$trainSet
  tuneDataFrame=trainTuneDFs$listDf[[counter02a]]$tuneSet
  tuneDataFrame%>%
    dplyr::select(conditionInvestigated)%>%
    tibble::rownames_to_column(var = "dataSet")->tuneConditionInvestigated
  tuneDataFrame %>%dplyr::select(-conditionInvestigated)->tuneDataFrame
  
  classWeightVector=trainTuneDFs$listDf[[counter02a]]$weightVec
  
  for(counter02b in 1:length(costList))
  {
    counter03=counter03+1;
    print(paste0(counter03, " / ", nrow(linearTuneResults), "/ Linear // RunNo: ", 
                 counter01, "/",numRepeatsFor_TestTrainSubset_Choice))
    costValue=linearTuneResults$costList[counter03]
    
    modelSVM_tune_linear<-e1071::svm(data = trainDataFrame,
                                     conditionInvestigated~.,
                                     type = type_svmChoice,
                                     kernel = "linear",
                                     class.weights = classWeightVector,
                                     cost=costValue)
    
    modelSVM_tune_linear %>%
      predict(.,tuneDataFrame)%>%
      data.frame(predictedValue = .) %>%
      tibble::rownames_to_column(var = "dataSet") %>%
      dplyr::left_join(tuneConditionInvestigated, ., by = "dataSet")->predictedResults
    
    
    if(type_svmChoice=="C-classification")
    {linearTuneResults$error.val[counter03]=F1ScoreErrCppCorrected(predictedResults$conditionInvestigated,
                                                                   predictedResults$predictedValue)}
    if(type_svmChoice=="eps-regression")
    {linearTuneResults$error.val[counter03]=1-
      summary(lm(predictedResults$conditionInvestigated~predictedResults$predictedValue))$r.squared}
    
  }
  
}


# B) PREDICT
linearTuneResults %>%
  dplyr::group_by(costList)%>%
  dplyr::summarise(meanErr=mean(error.val))%>%
  dplyr::group_by()%>%
  dplyr::filter(meanErr==min(meanErr)) %>%
  dplyr::sample_n(size=1)-> linearTuneWinner


linearTuneWinner %>% .$costList -> best_costValue_linear
linearTuneWinner %>% .$meanErr -> meanErr_linear;
best_performance_linear = 1 - meanErr_linear


# train all traintuneDf with best cost do predictions on test
dim_reduced_traintune_DF%>%
  dplyr::group_by(conditionInvestigated)%>%
  dplyr::summarise(numSamp=n())%>%
  dplyr::mutate(sumNumSamp=sum(numSamp))%>%
  dplyr::mutate(weight=sumNumSamp/numSamp)%>%
  dplyr::mutate(weight=weight/sum(weight))->temp

temp$weight->classWeightVector
names(classWeightVector) <- temp$conditionInvestigated

modelSVM_linear<-e1071::svm(data = dim_reduced_traintune_DF,
                            conditionInvestigated~.,
                            type = type_svmChoice,
                            kernel = "linear",
                            class.weights = classWeightVector,
                            cost=best_costValue_linear)

modelSVM_linear %>%
  predict(.,dim_reduced_test_DF) %>%
  data.frame(predictedValue = .) %>%
  tibble::rownames_to_column(var = "dataSet") %>%
  dplyr::left_join(meta_df_Test,.) %>%
  dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
  dplyr::mutate(TestTrainSubsetNo=counter01) -> modelSVM_linear


modelSVM_linear%>%
  dplyr::mutate(cost=best_costValue_linear) %>%
  dplyr::mutate(kernel="linear") %>%
  dplyr::mutate(performance=best_performance_linear) -> result_i_linear