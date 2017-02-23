# try 1
###*****************************
modelSVM_try1<-e1071::svm(formula=conditionInvestigated ~., 
                     data = dim_reduced_traintune_DF, 
                     type=type_svmChoice, 
                     class.weights =classWeightVector,
                     kernel=kernel_typeChoice, probability = TRUE)
###*****************************


###*****************************
# Do Predictions with models
modelSVM_try1 %>%
  predict(.,dim_reduced_test_DF) %>%
  data.frame(predictedValue = .) %>%
  tibble::rownames_to_column(var = "dataSet") %>%
  dplyr::left_join(meta_df_Test,.) %>%
  dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
  dplyr::mutate(TestTrainSubsetNo=counter01)->result_i
###*****************************

#try_2
###*****************************
# gamma vector
gammaList=10^seq(from=log10(1/dimensionChoice)-3,
                 to=log10(1/dimensionChoice)+3,
                 length.out =15)
# cost vector
costList=10^seq(from=-3,
                to=3,
                length.out =15)

# Error Function
FscoreErr<-function(y,prediction)
{
  score=sum(y==prediction)
}


tuneObj<-tune(method = svm, 
              conditionInvestigated~., 
              data = dim_reduced_traintune_DF,
              type = type_svmChoice, 
              kernel=kernel_typeChoice, 
              class.weights = classWeightVector,
              ranges = list(gamma = gammaList, cost = costList),
              tunecontrol = tune.control(best.model = TRUE,
                                         performances = FALSE,
                                         sampling="cross",
                                         cross=20,
                                         nrepeat = 1,
                                         error.fun = FscoreErr(y,prediction)))

modelSVM_try2<-tuneObj$best.model

modelSVM_try2 %>%
  predict(.,dim_reduced_test_DF) %>%
  data.frame(predictedValue = .) %>%
  tibble::rownames_to_column(var = "dataSet") %>%
  dplyr::left_join(meta_df_Test,.) %>%
  dplyr::mutate(TrueFalse=ifelse(predictedValue==conditionInvestigated,1,0)) %>%
  dplyr::mutate(TestTrainSubsetNo=counter01)->result_j
###*****************************