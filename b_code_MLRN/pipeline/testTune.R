a=3
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