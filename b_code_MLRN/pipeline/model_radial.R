# model radial

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
