modelRF<-randomForest::randomForest(conditionInvestigated~.,
                                    data = dim_reduced_traintune_DF, 
                                    importance=TRUE,
                                    do.trace=100,
                                    classwt=classWeightVectorRForest,
                                    nPerm=5,
                                    ntree=50000,
                                    mtry=5)
RFConfusion<-modelRF$confusion; 
RFConfusion<-as.data.frame(RFConfusion); 

RFConfusion %>%
  dplyr::select(-class.error)%>%
  tibble::rownames_to_column(var = "y")%>%
  tidyr::gather(key = prediction, value = repeatNumber, -y)->tidyRFConfusion

y_RF=rep(tidyRFConfusion$y,tidyRFConfusion$repeatNumber)
prediction_RF=rep(tidyRFConfusion$prediction,tidyRFConfusion$repeatNumber)
F1_err=F1ScoreErrCpp(y_RF,prediction_RF)
performance=1-F1_err
###*****************************


###*****************************
#With tuning
ntreelistRF=c(1000, 5000, 10000, 50000, 100000)
nodesizelistRF=c(1,2,3,4,5)
mtrylistRF=c(3,4,5,6,7)

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



