# RF_tune

###*****************************
RFTuneResults<-merge(data.frame(ntree=ntreelistRF),
                     merge(data.frame(mtry=mtrylistRF),
                           merge(data.frame(nodesize=nodesizelistRF),tryList,
                                 all=TRUE),
                           all=TRUE),
                     all=TRUE)
RFTuneResults%>%dplyr::mutate(error.val=NA)->RFTuneResults

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
  
  for(counter02b in 1:length(nodesizelistRF))
  {
    for(counter02c in 1:length(mtrylistRF))
    {
      for(counter02d in 1:length(ntreelistRF))
      {
        counter03=counter03+1;
        print(paste0(counter03, " / ", nrow(RFTuneResults), "/ RF // RunNo: ", 
                     counter01, "/",numRepeatsFor_TestTrainSubset_Choice))
        
        nodesizeValue=RFTuneResults$nodesize[counter03]
        mtryValue=RFTuneResults$mtry[counter03]
        ntreeValue=RFTuneResults$ntree[counter03]
        
        if(type_svmChoice=="C-classification")
        {modelSVM_tune_RF<-randomForest::randomForest(x=trainDataFrame[-1], y=factor(trainDataFrame[[1]]),
                                                      ntree= ntreeValue,
                                                      mtry=mtryValue,
                                                      nodesize=nodesizeValue,
                                                      classwt=classWeightVector)}
        
        if(type_svmChoice=="eps-regression")
        {modelSVM_tune_RF<-randomForest::randomForest(x=trainDataFrame[-1], y=trainDataFrame[[1]],
                                                      ntree= ntreeValue,
                                                      mtry=mtryValue,
                                                      nodesize=nodesizeValue,
                                                      classwt=classWeightVector)}
        
        
        
        modelSVM_tune_RF %>%
          predict(.,tuneDataFrame)%>%
          data.frame(predictedValue = .) %>%
          tibble::rownames_to_column(var = "dataSet") %>%
          dplyr::left_join(tuneConditionInvestigated, ., by = "dataSet")->predictedResults
        
        
        if(type_svmChoice=="C-classification")
        {RFTuneResults$error.val[counter03]=F1ScoreErrCppCorrected(predictedResults$conditionInvestigated,
                                                                   predictedResults$predictedValue)}
        if(type_svmChoice=="eps-regression")
        {RFTuneResults$error.val[counter03]=1-
          summary(lm(predictedResults$conditionInvestigated~predictedResults$predictedValue))$r.squared}
        
      }
    }
  }
  
}

# B) PREDICT
RFTuneResults %>%
  dplyr::group_by(ntree,mtry,nodesize)%>%
  dplyr::summarise(meanErr=mean(error.val))%>%
  dplyr::group_by()%>%
  dplyr::filter(meanErr==min(meanErr))%>%
  dplyr::sample_n(size=1) -> RFTuneWinner

RFTuneWinner %>% .$ntree -> best_ntreeValue_RF
RFTuneWinner %>% .$mtry -> best_mtryValue_RF
RFTuneWinner %>% .$nodesize -> best_nodesizeValue_RF
RFTuneWinner %>% .$meanErr -> meanErr_RF
best_performance_RF = 1 - meanErr_RF


# train all traintuneDf with best ntree, mtry, nodesize and do predictions on test
dim_reduced_traintune_DF%>%
  dplyr::group_by(conditionInvestigated)%>%
  dplyr::summarise(numSamp=n())%>%
  dplyr::mutate(sumNumSamp=sum(numSamp))%>%
  dplyr::mutate(weight=sumNumSamp/numSamp)%>%
  dplyr::mutate(weight=weight/sum(weight))->temp

temp$weight->classWeightVector
names(classWeightVector) <- temp$conditionInvestigated

if(type_svmChoice=="C-classification")
{modelSVM_RF<-randomForest::randomForest(x=dim_reduced_traintune_DF[-1], y=factor(dim_reduced_traintune_DF[[1]]),
                                         ntree= best_ntreeValue_RF,
                                         mtry=best_mtryValue_RF,
                                         nodesize=best_nodesizeValue_RF,
                                         classwt=classWeightVector)}

if(type_svmChoice=="eps-regression")
{modelSVM_RF<-randomForest::randomForest(x=dim_reduced_traintune_DF[-1], y=dim_reduced_traintune_DF[[1]],
                                         ntree= best_ntreeValue_RF,
                                         mtry=best_mtryValue_RF,
                                         nodesize=best_nodesizeValue_RF,
                                         classwt=classWeightVector)}


modelSVM_RF %>%
  predict(.,dim_reduced_test_DF) %>%
  data.frame(predictedValue = .) %>%
  tibble::rownames_to_column(var = "dataSet") %>%
  dplyr::left_join(meta_df_Test,.) %>%
  dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
  dplyr::mutate(TestTrainSubsetNo=counter01) %>%
  dplyr::mutate(ntreeValue=best_ntreeValue_RF) %>%
  dplyr::mutate(mtryValue=best_mtryValue_RF) %>%
  dplyr::mutate(nodesizeValue=best_nodesizeValue_RF) %>%
  dplyr::mutate(performance=best_performance_RF) -> result_i_RF
