# Machine learning main code part 1.

#Initial DataFrame preperation
###*****************************####
source("pipeline/machineLearning_subCode_initDfprep.R")
###*****************************####


# generate the data frame for runs
###*****************************####
# linear part
linear_df <- data.frame(model = "linear",
                        linear_cost_power = linear_range_cost_power)
linear_df %>%
  dplyr::mutate(linear_cost = 10 ^ linear_cost_power) -> linear_df


# radial part
radial_df <- expand.grid(radial_cost_power = radial_range_cost_power, 
                         radial_gamma_power = radial_range_gamma_power)

radial_df %>%
  dplyr::mutate(radial_cost = 10 ^ radial_cost_power,
                radial_gamma = 10 ^ radial_gamma_power,
                model = "radial") -> radial_df

# sigmoidal part
sigmoid_df <- expand.grid(sigmoid_cost_power = sigmoid_range_cost_power, 
                          sigmoid_gamma_power = sigmoid_range_gamma_power)

sigmoid_df %>%
  dplyr::mutate(sigmoid_cost = 10 ^ sigmoid_cost_power,
                sigmoid_gamma = 10 ^ sigmoid_gamma_power,
                model = "sigmoid") -> sigmoid_df

# RF part
RF_df <- expand.grid(ntree = ntree_list_RF, 
                     nodesize = node_size_list_RF,
                     mtry = mtry_list_RF)

RF_df %>%
  dplyr::mutate(model = "RF") -> RF_df

# Combine four models
dplyr::bind_rows(linear_df, radial_df, sigmoid_df, RF_df) %>%
  dplyr::mutate(run_no_for_each_data_division = seq(1:n())) -> combined_runs_df


TestTrainSubsetNo_df <- 
  expand.grid(
    run_no_for_each_data_division = combined_runs_df$run_no_for_each_data_division,
    TestTrainSubsetNo = seq(1:numRepeatsFor_TestTrainSubset_Choice),
    crossValue = seq(1:crossValue)
  )

dplyr::left_join(TestTrainSubsetNo_df, combined_runs_df) -> combined_runs_df

combined_runs_df %>%
  dplyr::mutate(run_no = seq(1:n())) -> combined_runs_df 

combined_runs_df$par_work <- rep(x = seq(1:ProcCount), 
                                 length.out = nrow(combined_runs_df))
###*****************************####


###*****************************####
# Geneate test vs train&tune meta-data
meta_df_list <- list()
main_df_list <- list()

for(counter01 in 1 : numRepeatsFor_TestTrainSubset_Choice)
{
  # Find out data sets that will go into machine learning algorithm
  #output <- divisionForTest(inputMetaDf,percentTest)
  output <- divisionForTestTune(inputMetaDf, percentTest, percentTune, crossValue)
  
  meta_df_Test = output$meta_df_Test # represent the test set
  meta_df_TrainTune = output$meta_df_TrainTune # represent the train set
  
  meta_df_Test %>% dplyr::arrange(sampleNum)-> meta_df_Test # put test set in order
  meta_df_TrainTune %>% dplyr::arrange(sampleNum)-> meta_df_TrainTune # put train set in order
  
  
  meta_df_list$meta_df_Test[[counter01]] <- meta_df_Test
  meta_df_list$meta_df_TrainTune[[counter01]] <- meta_df_TrainTune
  
  remove(output)
}
###*****************************####

browser()
###*****************************####
# Save files
# write.csv(x = combined_runs_df, file = paste0("pipeline_data/runlist_", analyzeName, "_part1.csv"))
# it generates unnecessary large files in the size of 30mb (I do not want to keep them.)

save(list = c("metaRunParameters","combined_runs_df", "mainDataFrame", "meta_df_list"), 
     file = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA"), 
     compression_level = 9, compress = "xz")
###*****************************####
browser()

###*****************************####
#generate part1 flag
part1_flag = 1
source("pipeline/machineLearning_mainCode_part2_carefulPick.R")
###*****************************####