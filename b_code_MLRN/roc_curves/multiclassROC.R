multiclassROC<-function(probabilitiesDF, groundTruth_DF, distinctRunNo = 0, model_Name=NULL)
{
  probabilitiesDF %>%
    tibble::rownames_to_column(var = "dataSet")%>%
    tidyr::gather(key="prediction", value = "probability",-dataSet)->probabilities_tidy
  
  meta_df_Test %>%
    dplyr::select(dataSet, ground_truth = conditionInvestigated) %>%
    dplyr::left_join(probabilities_tidy,.)->probabilities_tidy
  
  for(counter02 in 1:ncol(probabilitiesDF))
  {
    chosenCondition=as.vector(classWeightDf$conditionInvestigated[counter02])
    
    probabilities_tidy %>%
      dplyr::mutate(twoLevelPrediction=ifelse(chosenCondition==prediction,
                                              "predicted_as_selected_cond",
                                              "predicted_as_NOT_selected_cond")) %>%
      dplyr::mutate(twoLevelGroundTruth=ifelse(chosenCondition==ground_truth,
                                               "selected_cond",
                                               "NOT_selected_cond"))->probabilities_sub_tidy
    
    probabilities_sub_tidy%>%
      dplyr::group_by(dataSet,twoLevelPrediction)%>%
      dplyr::summarize(probability=max(probability), 
                       twoLevelGroundTruth=unique(twoLevelGroundTruth))%>%
      dplyr::group_by(dataSet)%>%
      dplyr::mutate(probability=probability/sum(probability))-> probabilities_sub_tidy_summary
    
    probabilities_sub_tidy_summary %>%
      tidyr::spread(key = twoLevelPrediction, value = probability)->probabilities_sub_summary
    
    rownames(probabilities_sub_summary)<-as.vector(probabilities_sub_summary$dataSet)
    
    probabilitiesVec=as.vector(probabilities_sub_summary$predicted_as_selected_cond)
    twoLevelGroundTruth=as.vector(probabilities_sub_summary$twoLevelGroundTruth)
    
    
    rocr <- ROCR::prediction( as.vector(probabilitiesVec), factor(twoLevelGroundTruth))
    x.perf <- ROCR::performance(rocr, "tpr","fpr")
    
    plot(x.perf, col=6)
    
    temp<-as.data.frame(x = t(c(distinctRuns=distinctRunNo,
                                model=model_Name, 
                                chosenCondition=chosenCondition)))
    
    temp$alpha.values=slot(x.perf,"alpha.values")
    temp$x.values=slot(x.perf,"x.values")
    temp$y.values=slot(x.perf,"y.values")
    
    AllResults=rbind(AllResults,temp)
  }
  
  return(AllResults)
}
