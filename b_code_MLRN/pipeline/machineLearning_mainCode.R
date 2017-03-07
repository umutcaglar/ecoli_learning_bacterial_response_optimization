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
parallel_Result <- foreach(counter01=1:numRepeatsFor_TestTrainSubset_Choice) %dopar%
{
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
  
  classWeightVector<-as.vector(sum(classWeightDf$samSize)/classWeightDf$samSize)
  names(classWeightVector)<-classWeightDf$conditionInvestigated
  classWeightVectorRForest=classWeightVector/sum(classWeightVector)
  ###*****************************
  
  
  # Generate Gamma Cost and kernel vectors
  ###*****************************
  # gamma vector
  gammaList=10^seq(from=log10(1/dimensionChoice)+powerRangeGammaLow,
                   to=log10(1/dimensionChoice)+powerRangeGammaHigh,
                   length.out = ndivision)
  # cost vector
  costList=10^seq(from=powerRangeCostLow,
                  to=powerRangeCostHigh,
                  length.out = ndivision)
  ###*****************************
  
  
  ###*****************************
  # tune svm for gamma, cost, kernel
  tuneObjSVM<-e1071::tune(method = svm,
                       conditionInvestigated~.,
                       data = dim_reduced_traintune_DF,
                       type = type_svmChoice,
                       class.weights = classWeightVector,
                       ranges = list(gamma = gammaList, cost =costList, kernel = kernelList),
                       tunecontrol = tune.control(best.model = TRUE,
                                                  performances = TRUE,
                                                  sampling=samplingValue,
                                                  cross=crossValue,
                                                  nrepeat = nrepeatValue,
                                                  error.fun = F1ScoreErrCpp))
  ###*****************************
  
  
  ###*****************************
  # extracting parameters
  modelSVM<-tuneObjSVM$best.model
  gammaSVM=tuneObjSVM$best.parameters$gamma
  costSVM=tuneObjSVM$best.parameters$cost
  kernelSVM=as.vector(tuneObjSVM$best.parameters$kernel)
  performanceSVM=1-tuneObjSVM$best.performance
  performanceDfSVM=dplyr::mutate(as.data.frame(tuneObjSVM$performances),runNum=counter01)
  ###*****************************
  
  
  ###*****************************
  # making predictions with best model
  modelSVM %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01) %>%
    dplyr::mutate(gamma=gammaSVM) %>%
    dplyr::mutate(cost=costSVM) %>%
    dplyr::mutate(kernel=kernelSVM) %>%
    dplyr::mutate(performance=performanceSVM) -> result_i_SVM
  ###*****************************
  
  
  ###*****************************
  counter02=0;
  flag01=0
  while(flag01 == 0 & counter02<=100){
    counter02=counter02+1;
    tuneObjRF<-try(e1071::tune.randomForest(x=dim_reduced_traintune_DF[-1], 
                                            y=factor(dim_reduced_traintune_DF[[1]]), 
                                            ntree= ntreelistRF,
                                            mtry=mtrylistRF,
                                            nodesize=nodesizelistRF,
                                            tunecontrol = tune.control(best.model = TRUE,
                                                                       performances = TRUE,
                                                                       sampling=samplingValue,
                                                                       cross=crossValue,
                                                                       nrepeat = nrepeatValue,
                                                                       error.fun = F1ScoreErrCpp)))
    
    print(paste0(counter02,"_",class(tuneObjRF)))
    if(class(tuneObjRF)!="try-error"){flag01=1}
  }
  ###*****************************
  
  
  ###*****************************
  # extracting parameters
  modelRF<-tuneObjRF$best.model
  nodesizeRF<-tuneObjRF$best.parameters$nodesize
  mtryRF<-tuneObjRF$best.parameters$mtry
  ntreeRF<-as.vector(tuneObjRF$best.parameters$ntree)
  performanceRF<-1-tuneObjRF$best.performance
  performanceDfRF<-dplyr::mutate(as.data.frame(tuneObjRF$performances),runNum=counter01)
  ###*****************************
  
  ###*****************************
  # making predictions with best model
  modelRF %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01) %>%
    dplyr::mutate(nodesize=nodesizeRF) %>%
    dplyr::mutate(mtry=mtryRF) %>%
    dplyr::mutate(ntree=ntreeRF) %>%
    dplyr::mutate(performance=performanceRF) -> result_i_RF
  ###*****************************
  
  
  ###*****************************
  # Make model RF Smaller (It can not make predictions anymore)
  modelRF$forest$nodestatus<-NULL
  modelRF$forest$bestvar<-NULL
  modelRF$forest$treemap<-NULL
  modelRF$forest$nodepred<-NULL
  modelRF$forest$xbestsplit<-NULL
  modelRF$forest$xlevels<-NULL
  ###*****************************
  
  
  
  # Parallel Way of combining data
  #******************************************
  # generate the list for output
  list(resultListSVM=result_i_SVM,
       performanceDfSVM=performanceDfSVM,
       #modelsvm=modelSVM,
       #modelrf=modelRF,
       resultListRF=result_i_RF,
       performanceDfRF=performanceDfRF)
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
timeStampFileOld<-read.csv(file = paste0("../b_results/","parametersCombined",".csv")) #import old file
timeStampFileNew<-dplyr::bind_rows(timeStampFileOld,timeStampVector) # add new line
timeStampFileNew<-unique(timeStampFileNew) # remove duplicates
write.csv(x=timeStampFileNew, file=paste0("../b_results/","parametersCombined",".csv"),row.names = FALSE)
#******************************************
