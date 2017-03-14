# model sigmoid

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
