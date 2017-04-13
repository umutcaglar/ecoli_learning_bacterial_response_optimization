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
  names(classWeightVector)<-classWeightDf$conditionInvestigated
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
  
  ROC_linear_DF=multiclassROC(probabilitiesDF = probabilitiesDF, 
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
  
  
  ROC_radial_DF=multiclassROC(probabilitiesDF = probabilitiesDF, 
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
  
  
  ROC_sigmoidal_DF=multiclassROC(probabilitiesDF = probabilitiesDF, 
                                 groundTruth_DF = ground_truth_DF, 
                                 distinctRunNo = counter01, 
                                 model_Name="sigmoidal")
  ###*****************************
  
  
  
  # model 4 -> RF
  ###*****************************
  model_RF<-randomForest::randomForest(x=dim_reduced_train_DF[-1], y=factor(dim_reduced_train_DF[[1]]),
                                       ntree= ntree_RF,
                                       mtry=mtry_RF,
                                       nodesize=nodesize_RF,
                                       classwt=classWeightVector)
  
  prediction_RF_DF <- predict(model_RF, 
                              type="prob", 
                              newdata=dim_reduced_test_DF, 
                              probability = TRUE)
  
  
  probabilitiesDF = as.data.frame(prediction_RF_DF)
  
  
  ROC_RF_DF=multiclassROC(probabilitiesDF = probabilitiesDF, 
                          groundTruth_DF = ground_truth_DF, 
                          distinctRunNo = counter01, 
                          model_Name="RF")
  ###*****************************
  
  
  # Combine Results
  ###*****************************
  if(counter01==1)
  {ROC_DF = dplyr::bind_rows(ROC_linear_DF, ROC_radial_DF, ROC_sigmoidal_DF, ROC_RF_DF)}
  if(counter01!=1)
  {ROC_DF = dplyr::bind_rows(ROC_DF,ROC_linear_DF, ROC_radial_DF, ROC_sigmoidal_DF, ROC_RF_DF)}
  ###*****************************
}

###*****************************####
# --END OF MAIN LOOP--


save(file = paste0("roc_curves/",testName,".Rdata"), ROC_DF)
