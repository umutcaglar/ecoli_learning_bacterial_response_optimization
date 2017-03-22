#Initial DataFrame preperation
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


# --MAIN LOOP--
# ###*****************************####
# counter03=0 # for division of the result frame to make the code more efficient
# tempmodelsvm_list=list()
# #******************************************

browser()
#******************************************
# --MAIN LOOP-- do the parallel processing
parallel_Result <- foreach(counter01=1:numRepeatsFor_TestTrainSubset_Choice) %do%
{
  print(counter01)
  # Find out data sets that will go into machine learning algorithm
  output<-divisionForTest(inputMetaDf,percentTest)

  meta_df_Test=output$inputDf_Test # represent the test set
  meta_df_TrainTune=output$inputDf_Train # represent the train set

  meta_df_Test %>% dplyr::arrange(sampleNum)-> meta_df_Test # put test set in order
  meta_df_TrainTune %>% dplyr::arrange(sampleNum)-> meta_df_TrainTune # put train set in order

  remove(output)
  ###*****************************


  ###*****************************
  # Divide the data as train and test
  mainDataFrame[,as.vector(meta_df_TrainTune$dataSet)]->traintuneDataFrame
  mainDataFrame[,as.vector(meta_df_Test$dataSet)]->testDataFrame
  ###*****************************


  ###*****************************
  # insert data preperation function
  dim_reduced_DF_obj<-dataPrepearningFunction(meta_df_Train = meta_df_TrainTune,
                                              meta_df_Test = meta_df_Test,
                                              trainDataFrame = traintuneDataFrame,
                                              testDataFrame = testDataFrame,
                                              dataNameDF = dataNameDF,
                                              dimReductionType = dimReductionType)

  dim_reduced_traintune_DF = dim_reduced_DF_obj$dim_reduced_train_DF
  dim_reduced_test_DF = dim_reduced_DF_obj$dim_reduced_test_DF
  dimensionChoice = dim_reduced_DF_obj$dimensionChoice
  ###*****************************


  ###*****************************
  # Generate Class Weight Vector
  meta_df_TrainTune %>%
    dplyr::group_by(conditionInvestigated)%>%
    dplyr::summarize(samSize=length(conditionInvestigated))->classWeightDf
  if(any(classWeightDf$samSize==1)){stop("no division in between train-tune is possible")}


  tryList=data.frame(tryList=paste0("test_",seq(1,crossValue)),
                     tryListNo=seq(1,crossValue))
  classWeightExpandDf<-merge(classWeightDf,tryList, all=TRUE)
  classWeightExpandDf %>%
    dplyr::group_by(conditionInvestigated,tryList)%>%
    dplyr::mutate(nTune=ifelse(samSize==2,as.vector(sample(0:1,1),mode="numeric"),-1),
                  nTune=ifelse(samSize>2,ceiling(samSize*percentTune),nTune))%>%
    dplyr::mutate(nTrain=samSize-nTune)->classWeightExpandDf


  classWeightExpandDf %>%
    dplyr::group_by(tryList) %>%
    dplyr::mutate(weight=sum(nTrain)/nTrain) %>% # weights should be higher for dmsll samples
    dplyr::mutate(weight=weight/sum(weight))->classWeightExpandDf # sum of the weights should be equal to one


  meta_df_TrainTune_Expand<-merge(meta_df_TrainTune,tryList, all=TRUE)
  meta_df_TrainTune_Expand <- left_join(meta_df_TrainTune_Expand,classWeightExpandDf)


  mutate_trainORtune_Function <-function(counter,nTrain)
  {
    selection=sample(counter,unique(nTrain))
    vec=rep("tune",length(counter))
    vec[selection]<-"train"
    return(vec)
  }
  ###*****************************


  ###*****************************
  # Expand meta DF
  meta_df_TrainTune_Expand %>%
    dplyr::arrange(tryList, conditionInvestigated)%>%
    dplyr::group_by(conditionInvestigated, tryList) %>%
    dplyr::mutate(counter=seq(1:n()))%>%
    dplyr::mutate(trainORtune=mutate_trainORtune_Function(counter,nTrain))->meta_df_TrainTune_Expand
  ###*****************************


  ###*****************************
  # Generate train tune data frames in trainTuneDFs
  do_trainANDtune_function<-function(trainORtune, dataSet, weight, conditionInvestigated)
  {
    data.frame(dataSet=dataSet,trainORtune=trainORtune)->mini_traintuneDF
    dim_reduced_traintune_DF %>%
      tibble::rownames_to_column(var = "dataSet")%>%
      dplyr::left_join(.,mini_traintuneDF)->dim_reduced_traintune_DF2

    dim_reduced_traintune_DF2 %>%
      dplyr::filter(trainORtune=="train")%>%
      tibble::column_to_rownames(var = "dataSet")%>%
      dplyr::select(-trainORtune)->trainSet
    dim_reduced_traintune_DF2 %>%
      dplyr::filter(trainORtune=="tune")%>%
      tibble::column_to_rownames(var = "dataSet")%>%
      dplyr::select(-trainORtune)->tuneSet

    data.frame(conditionInvestigated=conditionInvestigated,weight=weight) %>%
      dplyr::group_by(conditionInvestigated) %>%
      dplyr::summarize(weight=unique(weight))-> temp

    temp$weight->weightVec
    names(weightVec)<-temp$conditionInvestigated

    listObj=list(trainSet=trainSet,tuneSet=tuneSet, weightVec=weightVec)
    return(listObj)
  }

  meta_df_TrainTune_Expand%>%
    dplyr::group_by(tryList,tryListNo)%>%
    dplyr::do(listDf=do_trainANDtune_function(.$trainORtune,as.vector(.$dataSet), .$weight, .$conditionInvestigated))%>%
    dplyr::arrange(tryListNo)->trainTuneDFs
  ###*****************************


  # Generate Gamma and Cost vectors
  ###*****************************
  # gamma vector
  gammaList=10^seq(from=log10(1/dimensionChoice)+powerRangeGammaLow,
                   to=log10(1/dimensionChoice)+powerRangeGammaHigh,
                   length.out = ndivisionGamma)
  # cost vector
  costList=10^seq(from=powerRangeCostLow,
                  to=powerRangeCostHigh,
                  length.out = ndivisionCost)
  ###*****************************



  # model 1 -> "linear"
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
        dplyr::left_join(tuneConditionInvestigated,.)->predictedResults

      
      if(type_svmChoice=="C-classification")
      {linearTuneResults$error.val[counter03]=F1ScoreErrCpp(predictedResults$conditionInvestigated,
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

  ###*****************************


  # model 2 -> "radial"
  ###*****************************
  # A) TUNE

  radialTuneResults<-merge(data.frame(gammaList=gammaList),
                           merge(data.frame(costList=costList),
                                 tryList, all=TRUE),
                           all=TRUE)
  radialTuneResults%>%dplyr::mutate(error.val=NA)->radialTuneResults

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
        costValue=radialTuneResults$costList[counter03]
        gammaValue=radialTuneResults$gammaList[counter03]

        modelSVM_tune_radial<-e1071::svm(data = trainDataFrame,
                                         conditionInvestigated~.,
                                         type = type_svmChoice,
                                         kernel = "radial",
                                         class.weights = classWeightVector,
                                         cost=costValue,
                                         gamma=gammaValue)

        modelSVM_tune_radial %>%
          predict(.,tuneDataFrame)%>%
          data.frame(predictedValue = .) %>%
          tibble::rownames_to_column(var = "dataSet") %>%
          dplyr::left_join(tuneConditionInvestigated,.)->predictedResults

        
        if(type_svmChoice=="C-classification")
        {radialTuneResults$error.val[counter03]=F1ScoreErrCpp(predictedResults$conditionInvestigated,
                                                             predictedResults$predictedValue)}
        if(type_svmChoice=="eps-regression")
        {radialTuneResults$error.val[counter03]=1-
          summary(lm(predictedResults$conditionInvestigated~predictedResults$predictedValue))$r.squared}

      }
    }

  }

  # B) PREDICT
  radialTuneResults %>%
    dplyr::group_by(costList,gammaList)%>%
    dplyr::summarise(meanErr=mean(error.val))%>%
    dplyr::group_by()%>%
    dplyr::filter(meanErr==min(meanErr))%>%
    dplyr::sample_n(size=1)->radialTuneWinner

  radialTuneWinner %>% .$costList -> best_costValue_radial
  radialTuneWinner %>% .$gammaList -> best_gammaValue_radial
  radialTuneWinner %>% .$meanErr -> meanErr_radial
  best_performance_radial = 1 - meanErr_radial

  # train all traintuneDf with best cost and gamma do predictions on test
  dim_reduced_traintune_DF%>%
    dplyr::group_by(conditionInvestigated)%>%
    dplyr::summarise(numSamp=n())%>%
    dplyr::mutate(sumNumSamp=sum(numSamp))%>%
    dplyr::mutate(weight=sumNumSamp/numSamp)%>%
    dplyr::mutate(weight=weight/sum(weight))->temp

  temp$weight->classWeightVector
  names(classWeightVector) <- temp$conditionInvestigated

  modelSVM_radial<-e1071::svm(data = dim_reduced_traintune_DF,
                              conditionInvestigated~.,
                              type = type_svmChoice,
                              kernel = "radial",
                              class.weights = classWeightVector,
                              cost=best_costValue_radial,
                              gamma=best_gammaValue_radial)

  modelSVM_radial %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01) %>%
    dplyr::mutate(cost=best_costValue_radial) %>%
    dplyr::mutate(gamma=best_gammaValue_radial) %>%
    dplyr::mutate(kernel="radial") %>%
    dplyr::mutate(performance=best_performance_radial) -> result_i_radial
  ###*****************************


  # model 3 -> "sigmoid"
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
          dplyr::left_join(tuneConditionInvestigated,.)->predictedResults


        if(type_svmChoice=="C-classification")
        {sigmoidTuneResults$error.val[counter03]=F1ScoreErrCpp(predictedResults$conditionInvestigated,
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
  ###*****************************

  # model 4 -> RF
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
            dplyr::left_join(tuneConditionInvestigated,.)->predictedResults

          
          if(type_svmChoice=="C-classification")
          {RFTuneResults$error.val[counter03]=F1ScoreErrCpp(predictedResults$conditionInvestigated,
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
  ###*****************************


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
}
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
