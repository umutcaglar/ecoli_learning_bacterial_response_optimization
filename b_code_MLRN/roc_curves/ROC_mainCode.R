#Initial DataFrame preperation
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


# --MAIN LOOP--
# ###*****************************####
# counter03=0 # for division of the result frame to make the code more efficient
# tempmodelsvm_list=list()
# #******************************************


#******************************************
# --MAIN LOOP-- do the parallel processing
AllResults=data.frame(matrix(vector(), 0, 3,
                             dimnames=list(c(), c("Date", "File", "User"))),
                      stringsAsFactors=F)

for(counter01 in 1:numRepeatsFor_TestTrainSubset_Choice) 
{
  print(paste0("counter01 :",counter01))
  # Find out data sets that will go into machine learning algorithm
  output<-divisionForTest(inputMetaDf,percentTest)
  
  meta_df_Test=output$inputDf_Test # represent the test set
  meta_df_Train=output$inputDf_Train # represent the train set
  
  meta_df_Test %>% dplyr::arrange(sampleNum)-> meta_df_Test # put test set in order
  meta_df_Train %>% dplyr::arrange(sampleNum)-> meta_df_Train # put train set in order
  
  remove(output)
  ###*****************************
  
  
  ###*****************************
  # Divide the data as train and test
  mainDataFrame[,as.vector(meta_df_Train$dataSet)]->trainDataFrame
  mainDataFrame[,as.vector(meta_df_Test$dataSet)]->testDataFrame
  ###*****************************
  
  
  ###*****************************
  # insert data preperation function
  dim_reduced_DF_obj<-dataPrepearningFunction(meta_df_Train = meta_df_Train,
                                              meta_df_Test = meta_df_Test,
                                              trainDataFrame = trainDataFrame,
                                              testDataFrame = testDataFrame,
                                              dataNameDF = dataNameDF,
                                              dimReductionType = dimReductionType)
  
  dim_reduced_train_DF = dim_reduced_DF_obj$dim_reduced_train_DF
  dim_reduced_test_DF = dim_reduced_DF_obj$dim_reduced_test_DF
  dimensionChoice = dim_reduced_DF_obj$dimensionChoice
  ###*****************************
  
  
  ###*****************************
  # Generate Class Weight Vector
  meta_df_Train %>%
    dplyr::group_by(conditionInvestigated)%>%
    dplyr::summarize(samSize=length(conditionInvestigated))->classWeightDf
  if(any(classWeightDf$samSize==1)){stop("no division in between train is possible")}
  
  classWeightDf%>%
    dplyr::mutate(weights=1/samSize)%>%
    dplyr::mutate(weights=weights/sum(weights))->classWeightDf
  
  classWeightDf%>%
    .$weights %>%as.vector(.)->classWeightVector
  names(classWeightVector)<-classWeightDfSub$conditionInvestigated
  ###*****************************
  
  
  ###*****************************
  # Generate ground truth DF
  meta_df_Test %>%
    dplyr::select(dataSet, ground_truth = conditionInvestigated) -> ground_truth_DF
  ###*****************************
  
  
  # model 1 -> "linear"
  ###***************************
  modelSVM_linear_sub<-e1071::svm(data = dim_reduced_train_DF,
                                  conditionInvestigated~.,
                                  type = type_svmChoice,
                                  kernel = "linear",
                                  class.weights = classWeightVector,
                                  cost=Cost_linear,
                                  probability = TRUE)
  
  prediction_linear_DF <- predict(modelSVM_linear_sub, 
                                  type="prob", 
                                  newdata=dim_reduced_test_DF, 
                                  probability = TRUE)
  
  
  probabilitiesDF = as.data.frame(attr(prediction_linear_DF, "probabilities"))
  
  multiclassROC(probabilitiesDF = probabilitiesDF, 
                groundTruth_DF = ground_truth_DF, 
                distinctRunNo = counter01, 
                model_Name="linear")
  ###*****************************
  
  
  # model 2 -> "radial"
  ###*****************************
  modelSVM_radial<-e1071::svm(data = dim_reduced_train_DF,
                                  conditionInvestigated~.,
                                  type = type_svmChoice,
                                  kernel = "radial",
                                  class.weights = classWeightVector,
                                  cost=Cost_radial,
                                  gamma=Gamma_radial,
                                  probability = TRUE)
  
  prediction_radial_DF <- predict(modelSVM_radial, 
                                  type="prob", 
                                  newdata=dim_reduced_test_DF, 
                                  probability = TRUE)
  
  
  probabilitiesDF = as.data.frame(attr(prediction_radial_DF, "probabilities"))
  

  multiclassROC(probabilitiesDF = probabilitiesDF, 
                groundTruth_DF = ground_truth_DF, 
                distinctRunNo = counter01, 
                model_Name="radial")
  ###*****************************
  
  
  # model 3 -> "sigmoid"
  ###*****************************
  modelSVM_sigmoidal<-e1071::svm(data = dim_reduced_train_DF,
                                  conditionInvestigated~.,
                                  type = type_svmChoice,
                                  kernel = "sigmoid",
                                  class.weights = classWeightVector,
                                  cost=Cost_sigmoidal,
                                  gamma=Gamma_sigmoidal,
                                  probability = TRUE)
  
  prediction_sigmoidal_DF <- predict(modelSVM_sigmoidal, 
                                  type="prob", 
                                  newdata=dim_reduced_test_DF, 
                                  probability = TRUE)
  
  
  probabilitiesDF = as.data.frame(attr(prediction_sigmoidal_DF, "probabilities"))
  
  
  multiclassROC(probabilitiesDF = probabilitiesDF, 
                groundTruth_DF = ground_truth_DF, 
                distinctRunNo = counter01, 
                model_Name="sigmoidal")
  ###*****************************
  
  
  # model 4 -> RF
  #print(paste0("model 4 -> RF"))
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
          #print(paste0(counter03, " / ", nrow(RFTuneResults), "/ RF // RunNo: ", 
          #             counter01, "/",numRepeatsFor_TestTrainSubset_Choice))
          
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
          {RFTuneResults$error.val[counter03]=F1ScoreErr1(predictedResults$conditionInvestigated,
                                                          predictedResults$predictedValue)}
          if(type_svmChoice=="eps-regression")
          {RFTuneResults$error.val[counter03]=1-
            summary(lm(predictedResults$conditionInvestigated~predictedResults$predictedValue))$r.squared}
          
        }
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
  ###*****************************
  
  
  # # Non parallel way of combining data
  # #******************************************
  # parallel_Result[counter01]=list(resultListSVM_linear=result_i_linear,
  #                                 resultListSVM_radial=result_i_radial,
  #                                 resultListSVM_sigmoid=result_i_sigmoid,
  #                                 resultListRF=result_i_RF,
  #                                 #modelsvm=modelSVM,
  #                                 #modelrf=modelRF,
  #                                 performanceDf_linear=linearTuneResults,
  #                                 performanceDf_radial=radialTuneResults,
  #                                 performanceDf_sigmoid=sigmoidTuneResults,
  #                                 performanceDfRF=RFTuneResults)
  # 
  # #******************************************
  
  
  # Parallel Way of combining data
  #******************************************
  # generate the list for output
  list(resultListSVM_linear=result_i_linear,
       resultListSVM_radial=result_i_radial,
       resultListSVM_sigmoid=result_i_sigmoid,
       resultListRF=result_i_RF,
       #modelsvm=modelSVM,
       #modelrf=modelRF,
       performanceDf_linear=linearTuneResults,
       performanceDf_radial=radialTuneResults,
       performanceDf_sigmoid=sigmoidTuneResults,
       performanceDfRF=RFTuneResults)
  #******************************************
  
  ###*****************************####
  # --END OF MAIN LOOP--
  
  
  
  #******************************************
  # Save the data
  source("pipeline/timeStamp.R")
  #******************************************
  
  
  #******************************************
  # Save the files
  # a) save the loops output object (that includes predictions best parameters and parameter distributions)
  save(list = c("timeStampVector","parallel_Result",
                "factorOrder","factorOrder_Short",
                "expandedGrid","expandedGrid_Short",
                "vectorList","vectorList_Short",
                "testConditions","inputMetaDf"),
       file = paste0("../b_results/",fileName,".Rda"), compress = "xz")
  
  # b) save the conditions to csv file
  timeStampFileOld<-read.csv(file = paste0("../b_results/","parametersModelFitMetafile",".csv")) #import old file
  timeStampFileNew<-dplyr::bind_rows(timeStampFileOld,timeStampVector) # add new line
  timeStampFileNew<-unique(timeStampFileNew) # remove duplicates
  write.csv(x=timeStampFileNew, file=paste0("../b_results/","parametersModelFitMetafile",".csv"),row.names = FALSE)
  #******************************************
  