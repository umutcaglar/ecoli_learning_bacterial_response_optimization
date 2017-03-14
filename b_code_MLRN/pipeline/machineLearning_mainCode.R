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
parallel_Result <- foreach(counter01=1:numRepeatsFor_TestTrainSubset_Choice) %do%
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
  if(any(classWeightDf$samSize==1)){stop("no division in between train-tune is possible")}
  
  tryList=data.frame(tryList=paste0("test_",seq(1,crossValue)))
  classWeightExpandDf<-merge(classWeightDf,tryList, all=TRUE)
  classWeightExpandDf %>%
    dplyr::group_by(conditionInvestigated,tryList)%>%
    dplyr::mutate(nTune=ifelse(samSize==2,as.vector(sample(0:1,1),mode="numeric"),-1),
                  nTune=ifelse(samSize>2,ceiling(samSize*percentTune),nTune))%>%
    dplyr::mutate(nTrain=samSize-nTune)->classWeightExpandDf
  
  classWeightExpandDf %>%
    dplyr::group_by(conditionInvestigated) %>%
    dplyr::mutate(trainWeight=sum(nTrain)/nTrain)->q
  
  
  classWeightVector<-as.vector(sum(classWeightDf$samSize)/classWeightDf$samSize)
  names(classWeightVector)<-classWeightDf$conditionInvestigated
  classWeightVectorRForest=classWeightVector/sum(classWeightVector)
  ###*****************************
  
  browser()
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
  
  
  # model 1 -> "linear"
  source("pipeline/model_linear.R")
  
  # model 2 -> "radial"
  source("pipeline/model_radial.R")
  
  # model 3 -> "sigmoid"
  source("pipeline/model_sigmoid.R")
  
  # model 4 -> RF
  source("pipeline/model_RF.R")
  
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
