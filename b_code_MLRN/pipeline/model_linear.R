# model linear

###*****************************

browser()
for(counter02 in 1:length(costList))
{
  a=3
}



# tune svm for cost
browser()
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