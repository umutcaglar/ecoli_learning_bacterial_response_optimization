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
  tuneObj<-e1071::tune(method = svm,
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
  modelSVM<-tuneObj$best.model
  gamma=tuneObj$best.parameters$gamma
  cost=tuneObj$best.parameters$cost
  kernel=as.vector(tuneObj$best.parameters$kernel)
  performance=1-tuneObj$best.performance
  performanceDf=dplyr::mutate(as.data.frame(tuneObj$performances),runNum=counter01)
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
    dplyr::mutate(gamma=gamma) %>%
    dplyr::mutate(cost=cost) %>%
    dplyr::mutate(kernel=kernel) %>%
    dplyr::mutate(performance=performance) -> result_i
  ###*****************************
  
  
  # Parallel Way of combining data
  #******************************************
  # generate the list for output
  list(resultList=result_i,
       performanceDf=performanceDf,
       modelsvm=modelSVM)
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
