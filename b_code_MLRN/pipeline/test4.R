chosenAxis=paste0("Axis.",seq(1:10))
dim_reduced_traintune_DF_long %>%
  dplyr::select(one_of(c("conditionInvestigated",chosenAxis)))->dim_reduced_traintune_DF
dim_reduced_test_DF_long %>%
  dplyr::select(one_of(chosenAxis))->dim_reduced_test_DF
dimensionChoice=10

###*****************************
# Generate Class Weight Vector
meta_df_TrainTune %>%
  dplyr::group_by(conditionInvestigated)%>%
  dplyr::summarize(samSize=length(conditionInvestigated))->classWeightDf

classWeightVector<-as.vector(sum(classWeightDf$samSize)/classWeightDf$samSize)
names(classWeightVector)<-classWeightDf$conditionInvestigated
###*****************************


# Generate Gamma Cost and 
###*****************************
# gamma vector
gammaList=10^seq(from=log10(1/dimensionChoice)-3,
                 to=log10(1/dimensionChoice)+3,
                 length.out = 25)
# cost vector
costList=10^seq(from=-3,
                to=3,
                length.out = 25)

# kernel vector
kernelList=c("linear","radial","sigmoid")

tuneObj<-e1071::tune(method = svm,
                     conditionInvestigated~.,
                     data = dim_reduced_traintune_DF,
                     type = type_svmChoice,
                     class.weights = classWeightVector,
                     ranges = list(gamma = gammaList, cost =costList, kernel = kernelList),
                     tunecontrol = tune.control(best.model = TRUE,
                                                performances = FALSE,
                                                sampling=samplingValue,
                                                cross=20,
                                                nrepeat = 5,
                                                error.fun = FScoreErr))

1-tuneObj$best.performance

best.model<-tuneObj$best.model

gamma=tuneObj$best.parameters$gamma
cost=tuneObj$best.parameters$cost
kernel=tuneObj$best.parameters$kernel

best.model %>%
  predict(.,dim_reduced_test_DF) %>%
  data.frame(predictedValue = .) %>%
  tibble::rownames_to_column(var = "dataSet") %>%
  dplyr::left_join(meta_df_Test,.) %>%
  dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
  dplyr::mutate(TestTrainSubsetNo=counter01) %>%
  dplyr::mutate(gamma=gamma) %>%
  dplyr::mutate(cost=cost) %>%
  dplyr::mutate(kernel=kernel) %>%
  dplyr::mutate(performance=performance)->result_i

