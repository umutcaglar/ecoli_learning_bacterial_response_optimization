#Initial DataFrame preperation
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


# --MAIN LOOP--
###*****************************####
counter03=0 # for division of the result frame to make the code more efficient
for(counter01 in 1:numRepeatsFor_TestTrainSubset_Choice)
{
  ###*****************************
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
  
  dim_reduced_traintune_DF_long = dim_reduced_DF_obj$dim_reduced_train_DF
  dim_reduced_test_DF_long = dim_reduced_DF_obj$dim_reduced_test_DF
  dimensionChoice_long = dim_reduced_DF_obj$dimensionChoice
  ###***************************** 
  browser()
  
  ###*****************************
  # Preperation for while loop to pick the best performing axis
  listGamma=0
  listCost=0
  listKernel=""
  listPerformance=0
  listModel=list("null")
  
  
  listAxis=0
  listScore=0
  lastScore=0
  counter02a=1
  
  
  # ---START OF THE WHILE LOOP ---
  while(counter02a==1|listScore[counter02a]>lastScore)
  {
    
    loopGamma=rep(x = 0,dimensionChoiceValue)
    loopCost=rep(x = 0,dimensionChoiceValue)
    loopKernel=rep(x = "",dimensionChoiceValue)
    loopScore=rep(x = 0,dimensionChoiceValue)
    loopModelSVM=list()
    
    for(counter02b in 1:dimensionChoiceValue)
    {
      if(any(listAxis==counter02b))
      {
        loopModelSVM[counter02b]<-NA
        loopGamma[counter02b]=NA
        loopCost[counter02b]=NA
        loopKernel[counter02b]=NA
        loopScore[counter02b]= NA
      }
      
      if(!any(listAxis==counter02b))
      {
        ###*****************************
        # Re filter the Axises
        if(length(listAxis)==1){chosenAxis=paste0("Axis.",counter02b)}
        if(length(listAxis)!=1){chosenAxis=paste0("Axis.",c(listAxis[2:length(listAxis)],counter02b))}
        
        dim_reduced_traintune_DF_long %>%
          dplyr::select(one_of(c("conditionInvestigated",chosenAxis)))->dim_reduced_traintune_DF
        dim_reduced_test_DF_long %>%
          dplyr::select(one_of(chosenAxis))->dim_reduced_test_DF
        dimensionChoice=length(listAxis)
        ###*****************************
        browser()
        
        # SVM
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
        gammaList=10^seq(from=log10(1/dimensionChoice)-3,
                         to=log10(1/dimensionChoice)+3,
                         length.out = 5)
        # cost vector
        costList=10^seq(from=-3,
                        to=3,
                        length.out = 5)
        
        # kernel vector
        kernelList=c("linear","radial","sigmoid")
        ###*****************************
        
        
        ###*****************************
        # tune svm for gamma, cost, kerne
        tuneObj<-e1071::tune(method = svm,
                             conditionInvestigated~.,
                             data = dim_reduced_traintune_DF,
                             type = type_svmChoice,
                             class.weights = classWeightVector,
                             ranges = list(gamma = gammaList, cost =costList, kernel = kernelList),
                             tunecontrol = tune.control(best.model = TRUE,
                                                        performances = FALSE,
                                                        sampling=samplingValue,
                                                        cross=crossValue,
                                                        nrepeat = nrepeatValue,
                                                        error.fun = F1ScoreErr))
        ###*****************************
        
        
        ###*****************************
        # Extract results from them
        loopModelSVM[[counter02b]]<-tuneObj$best.model
        
        loopGamma[counter02b]=tuneObj$best.parameters$gamma
        loopCost[counter02b]=tuneObj$best.parameters$cost
        loopKernel[counter02b]=as.vector(tuneObj$best.parameters$kernel)
        
        performance=1-tuneObj$best.performance
        loopScore[counter02b]= performance
        ###*****************************
        
        browser()
        # random forest
        # random forest sample
        q<-randomForest::randomForest(conditionInvestigated~.,
                                      data = dim_reduced_traintune_DF, 
                                      importance=TRUE,
                                      do.trace=100,
                                      classwt=classWeightVectorRForest,
                                      nPerm=5,
                                      ntree=50000,
                                      mtry=5)
        q1<-q$confusion; View(q1); q1<-as.data.frame(q1); 
        
        colnamesList=colnames(q1)
        colnamesList<-colnamesList[1:16]
        
        q1 %>%
          dplyr::select(-class.error)%>%
          tibble::rownames_to_column(var = "y")%>%
          tidyr::gather(key = prediction, value = repeatTime, -y)
        
        
        # ###*****************************
        # e1071::tune.randomForest(method = randomForest, 
        #                          conditionInvestigated~.,
        #                          data = dim_reduced_traintune_DF)
        # ###*****************************
        
      }
      print(counter02b)
    }
    
    chosenAxis=which(loopScore==max(loopScore,na.rm = T))
    listAxis[counter02a+1]=chosenAxis
    listScore[counter02a+1]=max(loopScore,na.rm = T)
    
    listGamma[counter02a+1] = loopGamma[chosenAxis]
    listCost[counter02a+1] = loopCost[chosenAxis]
    listKernel[counter02a+1] = loopKernel[chosenAxis]
    listModel[[counter02a+1]] = loopModelSVM[[chosenAxis]]
    
    lastScore=listScore[counter02a]
    
    counter02a=counter02a+1
    
    print(sort(listAxis))
    print(listScore)
  }
  # ---END OF THE WHILE LOOP ---
  axisList=listAxis[1:length(listAxis)-1]
  gamma = listGamma[length(listGamma)-1]
  cost = listCost[length(listCost)-1]
  kernel = listKernel[length(listKernel)-1]
  modelSVM = listModel[[length(listModel)-1]]
  performance = listScore[length(listScore)-1]
  
  browser()
  
  axisWeights=listScore[1:(length(listCost)-1)]-c(0,listScore[1:(length(listCost)-2)])
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
    dplyr::mutate(performance=performance) %>%
    dplyr::mutate(axisList=paste(axisList,collapse = "_")) %>%
    dplyr::mutate(axisWeight=paste(round(axisWeights,3),collapse = "_"))->result_i
  ###*****************************
  
  print(paste0("c :",cost,
               " gamma :", gamma,
               " kernel :", kernel,
               " performance :", performance))
  
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
    counter03=counter03+1
    assign(sprintf("tempList%03d", counter03),tempList)
    remove(tempList)
  }
  ###*****************************
  
  print(paste0("Counter01: ",counter01, "/", numRepeatsFor_TestTrainSubset_Choice))
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
source("pipeline/machineLearning_subCode_figure_save.R")
###*****************************
