###*****************************
# EXPLANATION
# Function For Data Choise 
# This function aims to generate lists for different trials 
# needs a vector of discreate conditions that will be investigated. 
###*****************************


###*****************************
divisionForTest <- function(inputDf,percentTest){
  
  
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
  if(is.factor(inputDf$conditionInvestigated)){
    if(min(inputDf_Summary$sampleNum_1_n)<=2)
    {stop("At least one condition have less than 3 samples")}}
  ###*****************************
  
  
  ###*****************************
  # find samples for test & control
  inputDf %>%
    dplyr::group_by(conditionInvestigated) %>%
    dplyr::sample_frac(size = percentTest, replace= FALSE)->inputDf_Test
  
  inputDf %>%
    dplyr::mutate(traintune_test = ifelse(sampleNum_1_n %in% inputDf_Test$sampleNum_1_n, 
                                      "test", "train&tune"))->inputDf
  
  inputDf %>%
    dplyr::filter(traintune_test=="train&tune")->inputDf_TrainTune
  ###*****************************
  
  
  ###*****************************
  return(list(inputDf_Test=inputDf_Test,
              inputDf_TrainTune=inputDf_TrainTune))
  ###*****************************
}
###*****************************


###*****************************
divisionForTune <- function(inputDf,percentTune)
{
  
  
  ###*****************************
  return(list(inputDf_Train=inputDf_Train,
              inputDf_Tune=inputDf_Train))
  ###*****************************
}
###*****************************



