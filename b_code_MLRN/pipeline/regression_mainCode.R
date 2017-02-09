#Initial DataFrame preperation
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


# --MAIN LOOP--
###*****************************####
counter02=0 # for division of the result frame to make the code more efficient
for(counter01 in 1:numRepeatsFor_TestTrainSubset_Choice)
{
  ###*****************************
  # Find out data sets that will go into machine learning algorithm
  output<-trialStructure(inputMetaDf,percentTest)
  
  meta_df_Test=output$inputDf_Test # represent the test set
  meta_df_Train=output$inputDf_Train # represent the train set
  
  meta_df_Train %>% dplyr::arrange(sampleNum)-> meta_df_Train # put train set in order
  meta_df_Test %>% dplyr::arrange(sampleNum)-> meta_df_Test # put test set in order
  
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
  
  
  # Class weights can not be used in regression
  if(type_svmChoice == "C-classification")
  {
    ###*****************************
    # Generate Class Weight Vector
    meta_df_Train %>%
      dplyr::group_by(conditionInvestigated)%>%
      dplyr::summarize(samSize=length(conditionInvestigated))->classWeightDf
    
    classWeightVector<-as.vector(sum(classWeightDf$samSize)/classWeightDf$samSize)
    names(classWeightVector)<-classWeightDf$conditionInvestigated
    ###*****************************
    
    
    ###*****************************
    # Generate Models by using e1071 with "svm" with "radial kernel"
    modelSVM<-e1071::svm(conditionInvestigated ~., 
                         data = dim_reduced_train_DF, 
                         type=type_svmChoice, 
                         class.weights =classWeightVector,
                         kernel=kernel_typeChoice, probability = TRUE)
    ###***************************** 
  }
  ###*****************************
  
  
  ###*****************************
  if(type_svmChoice == "eps-regression")
  {
    modelSVM<-e1071::svm(conditionInvestigated ~., 
                         data = dim_reduced_train_DF, 
                         type=type_svmChoice, 
                         kernel=kernel_typeChoice, probability = TRUE)
  }
  ###*****************************


  ###*****************************
  # Do Predictions with models
  modelSVM %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01)->result_i
  ###*****************************

  
  ###*****************************
  # the main time consuming part is the addition of multiple results together
  # to solve this problem 
  # I divide final list into small peaces
  if(counter01%%100==1)
  {
    tempList=result_i
  }
  
  if(counter01%%100!=1 & counter01%%100!=100)
  {
    tempList=dplyr::rbind_list(tempList,result_i)
  }
  
  if(counter01%%100==0)
  {
    counter02=counter02+1
    assign(sprintf("tempList%03d", counter02),tempList)
    remove(tempList)
  }
  ###*****************************
  
  print(counter01)
}
###*****************************####
# --END OF MAIN LOOP--


# --RE ORGANIZING OUTPUT OF LOOP-- ####
###*****************************
# Combining Peaces
result_List=dplyr::rbind_all(mget(grep("tempList*.*",ls(),value = TRUE)))
remove(list = (grep("tempList*.*",ls(),value = TRUE)))
###*****************************


###*****************************
#Make continious data continious
result_List$conditionInvestigated=as.numeric(result_List$conditionInvestigated)
result_List$predictedValue=as.numeric(result_List$predictedValue)
###*****************************
browser()


###*****************************
# Calculating Percentages for predictions and generate summary df.
result_List %>%
  dplyr::group_by(conditionInvestigated) %>%
  dplyr::summarise(conditionLength=length(conditionInvestigated)) %>%
  dplyr::left_join(result_List, .)->result_List


result_List %>%
  dplyr::group_by(conditionInvestigated,predictedValue) %>%
  dplyr::summarise(combinationLength=length(conditionInvestigated),
                   conditionLength=unique(conditionLength),
                   percentPrediction=100*combinationLength/conditionLength)->result_ListSum
###*****************************####


# Figures and save
###*****************************####
source("pipeline/machineLearning_subCode_figure_save_continious.R")
###*****************************
