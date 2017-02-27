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
  
  
  ###*****************************
  print(paste0("c :",cost,
               " gamma :", gamma,
               " kernel :", kernel,
               " performance :", performance))
  ###*****************************
  
  browser()
  
  
  # # Not parallel comnine of results
  # ###*****************************
  # # the main time consuming part is the addition of multiple results together
  # # to solve this problem 
  # # I divide final list into small peaces
  # if(counter01%%100==1)
  # {
  #   tempResultList=result_i
  #   tempPerformanceDfComb=performanceDf
  #   tempmodelsvm_list[[counter01]]<-modelSVM
  # }
  # 
  # if(counter01%%100!=1 & counter01%%100!=100)
  # {
  #   tempResultList=dplyr::rbind_list(tempResultList,result_i)
  #   tempPerformanceDfComb=dplyr::rbind_list(tempPerformanceDfComb,performanceDf)
  #   tempmodelsvm_list[[counter01]]<-modelSVM
  # }
  # 
  # if(counter01%%100==0)
  # {
  #   counter03=counter03+1
  #   assign(sprintf("tempResultList%03d", counter03),tempResultList)
  #   assign(sprintf("tempPerformanceDfComb%03d", counter03),tempPerformanceDfComb)
  #   assign(sprintf("tempmodelsvm_list%03d", counter03),tempmodelsvm_list)
  #   remove(tempResultList, tempPerformanceDfComb, tempmodelsvm_list)
  # }
  # ###*****************************
  
  # #print(paste0("Counter01: ",counter01, "/", numRepeatsFor_TestTrainSubset_Choice))
  
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

# 
# # --RE ORGANIZING OUTPUT OF LOOP-- ####
# ###*****************************
# # Combining Peaces
# result_List=dplyr::rbind_all(mget(grep("tempResultList*.*",ls(),value = TRUE)))
# remove(list = (grep("tempResultList*.*",ls(),value = TRUE)))
# ###*****************************
# 
# 
# ###*****************************
# # Calculating Percentages for predictions and generate summary df.
# result_List %>%
#   dplyr::group_by(conditionInvestigated) %>%
#   dplyr::summarise(conditionLength=length(conditionInvestigated)) %>%
#   dplyr::left_join(result_List, .)->result_List
# 
# 
# result_List %>%
#   dplyr::group_by(conditionInvestigated,predictedValue) %>%
#   dplyr::summarise(combinationLength=length(conditionInvestigated),
#                    conditionLength=unique(conditionLength),
#                    percentPrediction=100*combinationLength/conditionLength)->result_ListSum
# ###*****************************####
# 
# 
# # Figures and save
# ###*****************************####
# source("pipeline/machineLearning_subCode_figure_save.R")
# ###*****************************
