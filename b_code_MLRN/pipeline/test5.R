
###*****************************
dim_reduced_traintune_DF_long ->dim_reduced_traintune_DF
dim_reduced_test_DF_long -> dim_reduced_test_DF
dimensionChoice_long -> dimensionChoice
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
gammaList=10^seq(from=log10(1/dimensionChoice)-3,
                 to=log10(1/dimensionChoice)+3,
                 length.out = 35)
# cost vector
costList=10^seq(from=-3,
                to=3,
                length.out = 35)

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
                                                cross=10,
                                                nrepeat = 3,
                                                error.fun = F1ScoreErr))

tuneObj<-e1071::tune(method = svm,
                     conditionInvestigated~.,
                     data = dim_reduced_traintune_DF,
                     type = type_svmChoice,
                     class.weights = classWeightVector,
                     ranges = list(gamma = gammaList, cost =costList, kernel = kernelList),
                     tunecontrol = tune.control(best.model = TRUE,
                                                performances = FALSE,
                                                sampling=samplingValue,
                                                cross=10,
                                                nrepeat = 3,
                                                error.fun = F1ScoreErrCpp))
###*****************************





