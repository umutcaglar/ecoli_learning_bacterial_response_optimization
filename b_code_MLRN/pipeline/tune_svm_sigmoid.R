# SVM_sigmoid_tune

###*****************************
# A) TUNE
sigmoidTuneResults<-merge(data.frame(gammaList=gammaList),
                          merge(data.frame(costList=costList),
                                tryList, all=TRUE),
                          all=TRUE)
sigmoidTuneResults%>%dplyr::mutate(error.val=NA)->sigmoidTuneResults

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
    for(counter02c in 1:length(gammaList))
    {
      counter03=counter03+1;
      print(paste0(counter03, " / ", nrow(sigmoidTuneResults), "/ sigmoid // RunNo: ", 
                   counter01, "/",numRepeatsFor_TestTrainSubset_Choice))
      
      costValue=sigmoidTuneResults$costList[counter03]
      gammaValue=sigmoidTuneResults$gammaList[counter03]
      
      modelSVM_tune_sigmoid<-e1071::svm(data = trainDataFrame,
                                        conditionInvestigated~.,
                                        type = type_svmChoice,
                                        kernel = "sigmoid",
                                        class.weights = classWeightVector,
                                        cost=costValue,
                                        gamma=gammaValue)
      
      modelSVM_tune_sigmoid %>%
        predict(.,tuneDataFrame)%>%
        data.frame(predictedValue = .) %>%
        tibble::rownames_to_column(var = "dataSet") %>%
        dplyr::left_join(tuneConditionInvestigated, ., by = "dataSet")->predictedResults
      
      
      if(type_svmChoice=="C-classification")
      {sigmoidTuneResults$error.val[counter03]=F1ScoreErrCppCorrected(predictedResults$conditionInvestigated,
                                                                      predictedResults$predictedValue)}
      if(type_svmChoice=="eps-regression")
      {sigmoidTuneResults$error.val[counter03]=1-
        summary(lm(predictedResults$conditionInvestigated~predictedResults$predictedValue))$r.squared}
      
    }
  }
  
}

# B) PREDICT
sigmoidTuneResults %>%
  dplyr::group_by(costList,gammaList)%>%
  dplyr::summarise(meanErr=mean(error.val))%>%
  dplyr::group_by()%>%
  dplyr::filter(meanErr==min(meanErr))%>%
  dplyr::sample_n(size=1)->sigmoidTuneWinner

sigmoidTuneWinner %>% .$costList -> best_costValue_sigmoid
sigmoidTuneWinner %>% .$gammaList -> best_gammaValue_sigmoid
sigmoidTuneWinner %>% .$meanErr -> meanErr_sigmoid
best_performance_sigmoid = 1 - meanErr_sigmoid


# train all traintuneDf with best cost gamma and do predictions on test
dim_reduced_traintune_DF%>%
  dplyr::group_by(conditionInvestigated)%>%
  dplyr::summarise(numSamp=n())%>%
  dplyr::mutate(sumNumSamp=sum(numSamp))%>%
  dplyr::mutate(weight=sumNumSamp/numSamp)%>%
  dplyr::mutate(weight=weight/sum(weight))->temp

temp$weight->classWeightVector
names(classWeightVector) <- temp$conditionInvestigated

modelSVM_sigmoid<-e1071::svm(data = dim_reduced_traintune_DF,
                             conditionInvestigated~.,
                             type = type_svmChoice,
                             kernel = "sigmoid",
                             class.weights = classWeightVector,
                             cost=best_costValue_sigmoid,
                             gamma=best_gammaValue_sigmoid)

modelSVM_sigmoid %>%
  predict(.,dim_reduced_test_DF) %>%
  data.frame(predictedValue = .) %>%
  tibble::rownames_to_column(var = "dataSet") %>%
  dplyr::left_join(meta_df_Test,.) %>%
  dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
  dplyr::mutate(TestTrainSubsetNo=counter01) %>%
  dplyr::mutate(cost=best_costValue_sigmoid) %>%
  dplyr::mutate(gamma=best_gammaValue_sigmoid) %>%
  dplyr::mutate(kernel="sigmoid") %>%
  dplyr::mutate(performance=best_performance_sigmoid) -> result_i_sigmoid
