###*****************************
# EXPLANATION
# Function For Data Choise 
# This function aims to generate lists for different trials 
# needs a vector of discreate conditions that will be investigated. 
###*****************************


###*****************************
divisionForTest <- function(inputDf,percentTest, controlFlag = 1, 
                            label1 = "test", label2 = "train&tune"){
  
  
  # Add Additional Column
  inputDf %>%
    dplyr::mutate(sampleNum_1_n=seq(1,n()))->inputDf
  
  ###*****************************
  # Generate a summary 
  inputDf %>%
    dplyr::group_by(conditionInvestigated) %>%
    dplyr::summarise(sampleNum_1_n=length(conditionInvestigated))->inputDf_Summary
  ###*****************************
  
  ###*****************************
  # Warnings
  if(controlFlag==1)
  {
    if(is.factor(inputDf$conditionInvestigated)){
      if(min(inputDf_Summary$sampleNum_1_n)<=2)
      {stop("At least one condition have less than 3 samples")}} 
  }
  ###*****************************
  
  
  ###*****************************
  # find samples for test & control
  inputDf %>%
    dplyr::group_by(conditionInvestigated) %>%
    dplyr::sample_frac(size = percentTest, replace= FALSE)->inputDf_Test
  
  inputDf %>%
    dplyr::mutate(traintune_test = ifelse(sampleNum_1_n %in% inputDf_Test$sampleNum_1_n, 
                                          label1, label2))->inputDf
  
  inputDf %>%
    dplyr::filter(traintune_test==label2)->inputDf_TrainTune
  
  inputDf %>%
    dplyr::filter(traintune_test==label1)->inputDf_Test
  ###*****************************
  
  
  ###*****************************
  return(list(inputDf_Test=inputDf_Test,
              inputDf_TrainTune=inputDf_TrainTune))
  ###*****************************
}
###*****************************






###*****************************
divisionForTestTune <- function(inputDf, percentTest, percentTune, crossValue)
{
  output1 = divisionForTest(inputDf,percentTest)
  meta_df_Test = output1$inputDf_Test # represent the test set
  meta_df_TrainTune = output1$inputDf_Train # represent the train set
  
  numTuneSamples = round(nrow(inputDf) * percentTune)
  for(counter01 in 1:crossValue)
  {
    meta_df_TrainTune %>% 
      dplyr::select(dataSet, conditionInvestigated) -> shortTrainTune
    
    shortTrainTune %>%
      dplyr::group_by(conditionInvestigated) %>%
      dplyr::summarize(length = n()) -> summaryTrainTune
    
    dplyr::left_join(shortTrainTune ,summaryTrainTune) -> shortTrainTune
    
    shortTrainTune %>%
      dplyr::group_by(conditionInvestigated) %>%
      dplyr::sample_n(size = 1, replace= FALSE) -> tunePart1
    
    remainingSampleNumber = numTuneSamples - nrow(tunePart1)
    
    shortTrainTune %>%
      dplyr::group_by() %>%
      dplyr::filter(length>2) %>%
      dplyr::sample_n(size = remainingSampleNumber) -> tunePart2
    
    dplyr::bind_rows(tunePart1, tunePart2) %>% .$dataSet %>% as.vector(.) -> tuneData
    
    meta_df_TrainTune %>%
      dplyr::mutate(div_x = ifelse(dataSet %in% tuneData, "tune", "train")) -> meta_df_TrainTune
    
    div_x_loc = which(colnames(meta_df_TrainTune)== "div_x")
    colnames(meta_df_TrainTune)[div_x_loc] = sprintf("div_%02d", counter01)
  }
  
  return(list(meta_df_Test = meta_df_Test,
              meta_df_TrainTune = meta_df_TrainTune))
}
###*****************************



