# Machine learning main code part 2.


###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list= ls()[!(ls() %in% c("part1_flag", "analyzeName"))])
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response_optimization/b_code_MLRN/"))} # mac computer
###*****************************


###*****************************
# Analyze Name in case the run does to called from part1
source("pipeline/prepeare.R")
analyzeName_ <- "protein"
###*****************************


###*****************************
if(!exists(x = "part1_flag"))
{
  ###*****************************
  part1_flag = 0
  analyzeName <- analyzeName_
  file_location = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA")
  load(file = file_location)
  ###*****************************
}

if(exists(x = "part1_flag"))
{
  if (part1_flag == 1)
  {
    file_location = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA")
    load(file = file_location)
  }
  
  if (part1_flag == 0)
  {
    analyzeName <- analyzeName_
    file_location = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA")
    load(file = file_location)
  }
  
}
###*****************************


#********************************************
# ARRANGE BACKENDS
## use the multicore library
# a.
ProcCount <- 8 # registers specified number of workers  or
registerDoMC(ProcCount) # Or, reserve all all available cores
# b.
#registerDoMC()  # Automatically assign cores

getDoParWorkers() # check how many cores (workers) are registered
#********************************************


###*****************************
# The code will run in two steps (Part 2 is step1)

# 1. generate the training and test data with all the steps including 
#  -> PCA
#  -> SVA
#  -> calculate the weights
# 
# 2. the modified training and test data sets will be used to train
# multiple machine learning algorithms with desired parameters. 
# Results in form of F1 score will be stored 
###*****************************


###*****************************
# Some necessary individual parameters
numRepeatsFor_TestTrainSubset_Choice <- metaRunParameters$numRepeatsFor_TestTrainSubset_Choice
crossValue <- metaRunParameters$crossValue
dimReductionType <- metaRunParameters$dimReductionType
dimensionChoiceValue <- metaRunParameters$dimensionChoiceValue
###*****************************


###*****************************
# generate the datasets
dim_reduced_train_DF_list = list()
dim_reduced_tune_DF_list = list()
dim_reduced_test_DF_list = list()
dimensionChoice_list = list()
weight_vector_list = list()

parallel_Result=list()
parallel_Result <- foreach(counter01=1:numRepeatsFor_TestTrainSubset_Choice) %dopar%
#for(counter01 in 1 : numRepeatsFor_TestTrainSubset_Choice)
{
  dim_reduced_train_DF_sublist = list()
  dim_reduced_tune_DF_sublist = list()
  dim_reduced_test_DF_sublist = list()
  dimensionChoice_sublist = list()
  weight_vector_sublist = list()
  
  for(counter02 in 1:crossValue)
  {
    print(paste0("counter01: ",counter01,"  counter02: ", counter02))
    
    meta_df_TrainTune <- meta_df_list$meta_df_TrainTune[[counter01]]
    meta_df_Test <- meta_df_list$meta_df_Test[[counter01]]
    
    meta_df_TrainTune %>%
      dplyr::mutate_(div = sprintf("div_%02d", counter02)) -> meta_df_TrainTune_run
    
    meta_df_TrainTune_run %>%
      dplyr::filter(div == "train") -> meta_df_Train
    
    meta_df_TrainTune_run %>%
      dplyr::filter(div == "tune") -> meta_df_Tune
    
    # Step 1: Divide the data as train, tune and test
    mainDataFrame[,as.vector(meta_df_Train$dataSet)] -> trainDataFrame
    mainDataFrame[,as.vector(meta_df_Tune$dataSet)] -> tuneDataFrame
    mainDataFrame[,as.vector(meta_df_Test$dataSet)] -> testDataFrame
    
    # temproralily stick tune and test
    tunetestDataFrame <- bind_cols(tuneDataFrame, testDataFrame)
    row.names(tunetestDataFrame) <- rownames(tuneDataFrame)
    meta_df_TuneTest <- bind_rows(meta_df_Tune, meta_df_Test)
    
    # insert data preperation function. (Reduces dimension PCA , fSVA - batch correction)
    dim_reduced_DF_obj<-dataPrepearningFunction(meta_df_Train = meta_df_Train,
                                                meta_df_Test = meta_df_TuneTest,
                                                trainDataFrame = trainDataFrame,
                                                testDataFrame = tunetestDataFrame,
                                                dataNameDF = dataNameDF,
                                                dimReductionType = dimReductionType)
    
    dim_reduced_DF_obj$dim_reduced_train_DF -> dim_reduced_train_DF
    
    dim_reduced_DF_obj$dim_reduced_test_DF %>%
      tibble::rownames_to_column(var = "row_names") -> dim_reduced_tunetest_DF
    dim_reduced_tunetest_DF %>%
      dplyr::filter(row_names %in% as.vector(meta_df_Tune$dataSet)) %>%
      tibble::column_to_rownames(var = "row_names")-> dim_reduced_tune_DF
    dim_reduced_tunetest_DF %>%
      dplyr::filter(row_names %in% as.vector(meta_df_Test$dataSet)) %>%
      tibble::column_to_rownames(var = "row_names")-> dim_reduced_test_DF
    
    dimensionChoice = dim_reduced_DF_obj$dimensionChoice
    
    # Generate Class Weight Vector
    dim_reduced_train_DF %>%
      dplyr::group_by(conditionInvestigated) %>%
      dplyr::summarise(condition_count = n()) %>%
      dplyr::mutate(condition_weight = 1/condition_count) %>%
      dplyr::mutate(condition_weight_n = condition_weight / sum(condition_weight)) -> weight_df
    
    weight_df%>%
      .$condition_weight_n %>% as.vector(.)-> weight_vector
    
    names(weight_vector) <- as.vector(weight_df[["conditionInvestigated"]])
    
    dim_reduced_train_DF_sublist[[counter02]] = dim_reduced_train_DF
    dim_reduced_tune_DF_sublist[[counter02]] = dim_reduced_tune_DF
    dim_reduced_test_DF_sublist[[counter02]] = dim_reduced_test_DF
    dimensionChoice_sublist[[counter02]] = dimensionChoice
    weight_vector_sublist[[counter02]] = weight_vector
  }
  
  dim_reduced_train_DF_list[[counter01]] = dim_reduced_train_DF_sublist
  dim_reduced_tune_DF_list[[counter01]] = dim_reduced_tune_DF_sublist
  dim_reduced_test_DF_list[[counter01]] = dim_reduced_test_DF_sublist
  dimensionChoice_list[[counter01]] = dimensionChoice_sublist
  weight_vector_list[[counter01]] = weight_vector_sublist

  
  remove(list = c("dim_reduced_train_DF_sublist", 
                  "dim_reduced_tune_DF_sublist", 
                  "dim_reduced_test_DF_sublist", 
                  "dimensionChoice_sublist", 
                  "weight_vector_sublist"))
  
  
  # parallel Way of combining data
  list(dim_reduced_train_DF_list = dim_reduced_train_DF_list,
       dim_reduced_tune_DF_list = dim_reduced_tune_DF_list,
       dim_reduced_test_DF_list = dim_reduced_test_DF_list,
       dimensionChoice_list = dimensionChoice_list,
       weight_vector_list = weight_vector_list)
}

browser()

for(counter01 in 1: numRepeatsFor_TestTrainSubset_Choice)
{
  dim_reduced_train_DF_list[[counter01]] = parallel_Result[[counter01]]$dim_reduced_train_DF_list[[counter01]]
  dim_reduced_tune_DF_list[[counter01]] = parallel_Result[[counter01]]$dim_reduced_tune_DF_list[[counter01]]
  dim_reduced_test_DF_list[[counter01]] = parallel_Result[[counter01]]$dim_reduced_test_DF_list[[counter01]]
  dimensionChoice_list[[counter01]] = parallel_Result[[counter01]]$dimensionChoice_list[[counter01]]
  weight_vector_list[[counter01]] = parallel_Result[[counter01]]$weight_vector_list[[counter01]]
  print(counter01)
}
###*****************************####
# Save files
save(list = c("meta_df_list",
              "dim_reduced_train_DF_list",
              "dim_reduced_tune_DF_list",
              "dim_reduced_test_DF_list",
              "dimensionChoice_list",
              "weight_vector_list"), 
     file = paste0("pipeline_data/data_list_", analyzeName,"_part2.RDA"), 
     compression_level = 9, compress = "xz")

save(list = c("parallel_Result"), 
     file = paste0("pipeline_data/temp.RDA"), 
     compression_level = 9, compress = "xz")
###*****************************####


###*****************************####
#generate part2 flag
part2_flag = 1
source("pipeline/machineLearning_mainCode_part3_carefulPick.R")
###*****************************####