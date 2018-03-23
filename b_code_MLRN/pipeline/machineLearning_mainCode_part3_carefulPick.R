# Machine learning main code part 3.


###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list= ls()[!(ls() %in% c("part2_flag", "analyzeName"))])
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
if(!exists(x = "part2_flag"))
{
  ###*****************************
  part2_flag = 0
  analyzeName <- analyzeName_
  file_location = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA")
  load(file = file_location)
  file_location = paste0("pipeline_data/data_list_", analyzeName,"_part2.RDA")
  load(file = file_location)
  ###*****************************
}

if(exists(x = "part2_flag"))
{
  if (part2_flag == 2)
  {
    file_location = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA")
    load(file = file_location)
    file_location = paste0("pipeline_data/data_list_", analyzeName,"_part2.RDA")
    load(file = file_location)
  }
  
  if (part2_flag == 0)
  {
    analyzeName <- analyzeName_
    file_location = paste0("pipeline_data/runlist_", analyzeName, "_part1.RDA")
    load(file = file_location)
    file_location = paste0("pipeline_data/data_list_", analyzeName,"_part2.RDA")
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

# combined_runs_df$par_work <- rep(x = seq(1:ProcCount), 
#                                  length.out = nrow(combined_runs_df))
#********************************************


###*****************************
# The code will run in two steps (Part 3 is step2)

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
# SHOULD BE DELETED
#combined_runs_df[seq(1,10000),] -> combined_runs_df 
###*****************************

# browser()
###*****************************
# The function
f1_score_list <- foreach(counter01 = 1 : ProcCount) %dopar%
#
{
  combined_runs_df %>% dplyr::filter(par_work == counter01) -> combined_runs_df_sub
  f1_score_list_ = c()
  
  for(counter01 in 1: nrow(combined_runs_df_sub))
  {
    TestTrainSubsetNo <- combined_runs_df_sub[["TestTrainSubsetNo"]][counter01]
    crossValue <- combined_runs_df_sub[["crossValue"]][counter01]
    model <- combined_runs_df_sub[["model"]][counter01]
    linear_cost <- combined_runs_df_sub[["linear_cost"]][counter01]
    radial_cost <- combined_runs_df_sub[["radial_cost"]][counter01]
    radial_gamma <- combined_runs_df_sub[["radial_gamma"]][counter01]
    sigmoid_cost <- combined_runs_df_sub[["sigmoid_cost"]][counter01]
    sigmoid_gamma <- combined_runs_df_sub[["sigmoid_gamma"]][counter01]
    ntree_ <- combined_runs_df_sub[["ntree"]][counter01]
    nodesize_ <- combined_runs_df_sub[["nodesize"]][counter01]
    mtry_ <- combined_runs_df_sub[["mtry"]][counter01]
    
    train_data = dim_reduced_train_DF_list[[TestTrainSubsetNo]][[crossValue]]
    tune_data =  dim_reduced_tune_DF_list[[TestTrainSubsetNo]][[crossValue]]
    
    meta_df_list$meta_df_TrainTune[[TestTrainSubsetNo]] %>%
      dplyr::mutate_(div = sprintf("div_%02d", crossValue)) %>%
      dplyr::filter(div == "tune") -> meta_df_Tune
    
    
    weight_vector <- weight_vector_list[[TestTrainSubsetNo]][[crossValue]]
    
    
    if(model == "linear")
    {
      model <- e1071::svm(data = train_data,
                          conditionInvestigated ~ . ,
                          type = "C-classification",
                          kernel = model,
                          class.weights = weight_vector,
                          cost=linear_cost)
    }
    if(model == "radial")
    {
      model <- e1071::svm(data = train_data,
                          conditionInvestigated ~ . ,
                          type = "C-classification",
                          kernel = model,
                          class.weights = weight_vector,
                          cost = radial_cost,
                          gamma = radial_gamma)
    }
    
    if(model == "sigmoid")
    {
      model <- e1071::svm(data = train_data,
                          conditionInvestigated ~ . ,
                          type = "C-classification",
                          kernel = model,
                          class.weights = weight_vector,
                          cost = sigmoid_cost,
                          gamma = sigmoid_gamma)
    }
    
    if(model == "RF")
    {
      model <- randomForest::randomForest(x = train_data[-1], y = factor(train_data[[1]]),
                                          ntree = ntree_,
                                          mtry = mtry_,
                                          nodesize = nodesize_,
                                          classwt = weight_vector)
    }
    
    model %>%
      predict(.,tune_data)%>%
      data.frame(pred = .) %>%
      tibble::rownames_to_column(var = "dataSet") %>%
      dplyr::left_join(meta_df_Tune , ., by = "dataSet") %>%
      dplyr::select(dataSet, obs = conditionInvestigated, pred) -> predictedResults
    
    f1_score <- f1_multi_cond(predictedResults)
    f1_score_list_[counter01] = f1_score
  }
  f1_score_df <-data.frame(run_no = as.vector(combined_runs_df_sub$run_no), 
                          f1_score_list = f1_score_list_)
}
###*****************************
# browser()

object.size(combined_runs_df)
object.size(f1_score_list)

f1_score_df = c()
for(counter01 in 1: ProcCount)
{
  f1_score_df_sub <- f1_score_list[[counter01]]
  if(counter01 == 1)
  {
    f1_score_df = f1_score_df_sub
  }
  if(counter01 != 1)
  {
    f1_score_df = bind_rows(f1_score_df, f1_score_df_sub)
  }
}


f1_score_df %>% 
  dplyr::left_join(combined_runs_df, .) -> combined_runs_df

#******************************************
# Save the data
save(list = c("combined_runs_df"), 
     file = paste0("pipeline_data/f1_", analyzeName,"_part3.RDA"),
     compression_level = 9, compress = "xz")

load(file = paste0("pipeline_data/f1_", analyzeName,"_part3.RDA"))
#******************************************


combined_runs_df %>% 
  group_by(model, TestTrainSubsetNo,
           linear_cost, 
           radial_cost, radial_gamma, 
           sigmoid_cost, sigmoid_gamma, 
           ntree, nodesize, mtry) %>% 
  dplyr::summarise(f1_mean = mean(f1_score_list), f1_sd = sd(f1_score_list)) %>% 
  group_by(model,
           linear_cost, 
           radial_cost, radial_gamma, 
           sigmoid_cost, sigmoid_gamma, 
           ntree, nodesize, mtry) %>% 
  dplyr::summarise(f1_mean_median = median(f1_mean)) %>% 
  dplyr::group_by()%>%
  dplyr::arrange(desc(f1_mean_median)) %>% 
  dplyr::mutate(order = seq(1:n()))-> q2

q2 %>% dplyr::filter(model == "sigmoid") -> q3
