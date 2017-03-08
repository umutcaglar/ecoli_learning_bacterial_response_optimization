#Initial DataFrame preperation v_new
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


# --MAIN LOOP--
# ###*****************************####
# Divide main data frame into two parts
mainDataFrame %>%
  tibble::rownames_to_column()%>%
  dplyr::filter(grepl(pattern = "^ECB*.*", x = rowname))->mainDataFrame_mrna

mainDataFrame %>%
  tibble::rownames_to_column()%>%
  dplyr::filter(grepl(pattern = "^YP*.*", x = rowname))->mainDataFrame_protein

mainDataFrame_mrna %>% 
  tibble::column_to_rownames(df = ., var = "rowname")->mainDataFrame_mrna
mainDataFrame_protein %>% 
  tibble::column_to_rownames(df = ., var = "rowname")->mainDataFrame_protein
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
  mainDataFrame_mrna[,as.vector(meta_df_TrainTune$dataSet)]->traintuneDataFrame_mrna
  mainDataFrame_mrna[,as.vector(meta_df_Test$dataSet)]->testDataFrame_mrna
  
  mainDataFrame_protein[,as.vector(meta_df_TrainTune$dataSet)]->traintuneDataFrame_protein
  mainDataFrame_protein[,as.vector(meta_df_Test$dataSet)]->testDataFrame_protein
  ###*****************************
  
  
  ###*****************************
  if(batchCorrectionType=="separate")
  {
    # insert data preperation function overall
    dim_reduced_DF_obj<-dataPrepearningCombFunction(meta_df_Train = meta_df_TrainTune,
                                                    meta_df_Test = meta_df_Test,
                                                    trainDataFrame_mrna = traintuneDataFrame_mrna,
                                                    testDataFrame_mrna = testDataFrame_mrna,
                                                    trainDataFrame_protein = traintuneDataFrame_protein,
                                                    testDataFrame_protein = testDataFrame_protein,
                                                    dataNameDF = dataNameDF,
                                                    dimReductionType = dimReductionType)
    
    dim_reduced_traintune_DF = dim_reduced_DF_obj$dim_reduced_train_DF
    dim_reduced_test_DF = dim_reduced_DF_obj$dim_reduced_test_DF
    dimensionChoice = dim_reduced_DF_obj$dimensionChoice 
  }
  
  if(batchCorrectionType=="together")
  {
    # insert data preperation function
    trainDataFrame = dplyr::bind_rows(trainDataFrame_mrna, trainDataFrame_protein)	
    testDataFrame = dplyr::bind_rows(testDataFrame_mrna, testDataFrame_protein)
    
    dim_reduced_DF_obj<-dataPrepearningFunction(meta_df_Train = meta_df_TrainTune, 
                                                meta_df_Test = meta_df_Test, 
                                                trainDataFrame = traintuneDataFrame, 
                                                testDataFrame = testDataFrame, 
                                                dataNameDF = dataNameDF, 
                                                dimReductionType = dimReductionType)
    
    dim_reduced_traintune_DF = dim_reduced_DF_obj$dim_reduced_train_DF
    dim_reduced_test_DF = dim_reduced_DF_obj$dim_reduced_test_DF
    dimensionChoice = dim_reduced_DF_obj$dimensionChoice
  }
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
  
  
  # Kernel 1 -> "linear"
  ###*****************************
  # tune svm for cost
  tuneObjSVM_linear<-e1071::tune(method = svm,
                                 conditionInvestigated~.,
                                 data = dim_reduced_traintune_DF,
                                 type = type_svmChoice,
                                 kernel = "linear",
                                 class.weights = classWeightVector,
                                 ranges = list(cost =costList),
                                 tunecontrol = tune.control(best.model = TRUE,
                                                            performances = TRUE,
                                                            sampling=samplingValue,
                                                            cross=crossValue,
                                                            nrepeat = nrepeatValue,
                                                            error.fun = F1ScoreErrCpp))
  ###*****************************
  
  
  ###*****************************
  # extracting parameters
  modelSVM_linear<-tuneObjSVM_linear$best.model
  costSVM_linear=tuneObjSVM_linear$best.parameters$cost
  performanceSVM_linear=1-tuneObjSVM_linear$best.performance
  performanceDfSVM_linear=dplyr::mutate(as.data.frame(tuneObjSVM_linear$performances),runNum=counter01)
  ###*****************************
  
  
  ###*****************************
  # making predictions with best model
  modelSVM_linear %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01) %>%
    dplyr::mutate(cost=costSVM_linear) %>%
    dplyr::mutate(kernel="linear") %>%
    dplyr::mutate(performance=performanceSVM_linear) -> result_i_linear
  ###*****************************
  
  
  # Kernel 2 -> "radial"
  ###*****************************
  # tune svm for gamma, cost
  tuneObjSVM_radial<-e1071::tune(method = svm,
                                 conditionInvestigated~.,
                                 data = dim_reduced_traintune_DF,
                                 type = type_svmChoice,
                                 kernel = "radial",
                                 class.weights = classWeightVector,
                                 ranges = list(cost =costList, gamma=gammaList),
                                 tunecontrol = tune.control(best.model = TRUE,
                                                            performances = TRUE,
                                                            sampling=samplingValue,
                                                            cross=crossValue,
                                                            nrepeat = nrepeatValue,
                                                            error.fun = F1ScoreErrCpp))
  ###*****************************
  
  
  ###*****************************
  # extracting parameters
  modelSVM_radial<-tuneObjSVM_radial$best.model
  costSVM_radial=tuneObjSVM_radial$best.parameters$cost
  gammaSVM_radial=tuneObjSVM_radial$best.parameters$gamma
  performanceSVM_radial=1-tuneObjSVM_radial$best.performance
  performanceDfSVM_radial=dplyr::mutate(as.data.frame(tuneObjSVM_radial$performances),runNum=counter01)
  ###*****************************
  
  
  ###*****************************
  # making predictions with best model
  modelSVM_radial %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01) %>%
    dplyr::mutate(cost=costSVM_radial) %>%
    dplyr::mutate(gamma=gammaSVM_radial) %>%
    dplyr::mutate(kernel="radial") %>%
    dplyr::mutate(performance=performanceSVM_radial) -> result_i_radial
  ###*****************************
  
  
  # Kernel 3 -> "sigmoid"
  ###*****************************
  # tune svm for gamma, cost
  tuneObjSVM_sigmoid<-e1071::tune(method = svm,
                                  conditionInvestigated~.,
                                  data = dim_reduced_traintune_DF,
                                  type = type_svmChoice,
                                  kernel = "sigmoid",
                                  class.weights = classWeightVector,
                                  ranges = list(cost =costList, gamma=gammaList),
                                  tunecontrol = tune.control(best.model = TRUE,
                                                             performances = TRUE,
                                                             sampling=samplingValue,
                                                             cross=crossValue,
                                                             nrepeat = nrepeatValue,
                                                             error.fun = F1ScoreErrCpp))
  ###*****************************
  
  
  ###*****************************
  # extracting parameters
  modelSVM_sigmoid<-tuneObjSVM_sigmoid$best.model
  costSVM_sigmoid=tuneObjSVM_sigmoid$best.parameters$cost
  gammaSVM_sigmoid=tuneObjSVM_sigmoid$best.parameters$gamma
  performanceSVM_sigmoid=1-tuneObjSVM_sigmoid$best.performance
  performanceDfSVM_sigmoid=dplyr::mutate(as.data.frame(tuneObjSVM_sigmoid$performances),runNum=counter01)
  ###*****************************
  
  
  ###*****************************
  # making predictions with best model
  modelSVM_sigmoid %>%
    predict(.,dim_reduced_test_DF) %>%
    data.frame(predictedValue = .) %>%
    tibble::rownames_to_column(var = "dataSet") %>%
    dplyr::left_join(meta_df_Test,.) %>%
    dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
    dplyr::mutate(TestTrainSubsetNo=counter01) %>%
    dplyr::mutate(cost=costSVM_sigmoid) %>%
    dplyr::mutate(gamma=gammaSVM_sigmoid) %>%
    dplyr::mutate(kernel="sigmoid") %>%
    dplyr::mutate(performance=performanceSVM_sigmoid) -> result_i_sigmoid
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
  browser()
  
  
  # Parallel Way of combining data
  #******************************************
  # generate the list for output
  list(resultListSVM_linear=result_i_linear,
       resultListSVM_radial=result_i_radial,
       resultListSVM_sigmoid=result_i_sigmoid,
       resultListRF=result_i_RF,
       #modelsvm=modelSVM,
       #modelrf=modelRF,
       performanceDf_linear=performanceDfSVM_linear,
       performanceDf_radial=performanceDfSVM_radial,
       performanceDf_sigmoid=performanceDfSVM_sigmoid,
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
timeStampFileOld<-read.csv(file = paste0("../b_results/","parametersModelFitMetafile",".csv")) #import old file
timeStampFileNew<-dplyr::bind_rows(timeStampFileOld,timeStampVector) # add new line
timeStampFileNew<-unique(timeStampFileNew) # remove duplicates
write.csv(x=timeStampFileNew, file=paste0("../b_results/","parametersModelFitMetafile",".csv"),row.names = FALSE)
#******************************************
