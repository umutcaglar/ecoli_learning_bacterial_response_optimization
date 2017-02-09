dataPrepearningCombFunction<-function(meta_df_Train, meta_df_Test, 
                                      trainDataFrame_mrna, testDataFrame_mrna, 
                                      trainDataFrame_protein, testDataFrame_protein, 
                                      dataNameDF, dimReductionType)
{
  
  # ###*****************************
  # # add small random numbers
  # NCol<-ncol(trainDataFrame_mrna)
  # NRow<-nrow(trainDataFrame_mrna)
  # trainDataFrame_mrna <- trainDataFrame_mrna + 10^-7*as.data.frame(matrix(runif(n = NCol*NRow, min = -1, max = 1), ncol=NCol))
  # NCol<-ncol(trainDataFrame_protein)
  # NRow<-nrow(trainDataFrame_protein)
  # trainDataFrame_protein <- trainDataFrame_protein + 10^-7*as.data.frame(matrix(runif(n = NCol*NRow, min = -1, max = 1), ncol=NCol))
  # 
  # NCol<-ncol(testDataFrame_mrna)
  # NRow<-nrow(testDataFrame_mrna)
  # testDataFrame_mrna <- testDataFrame_mrna + 10^-7*as.data.frame(matrix(runif(n = NCol*NRow, min = -1, max = 1), ncol=NCol))
  # NCol<-ncol(testDataFrame_protein)
  # NRow<-nrow(testDataFrame_protein)
  # testDataFrame_protein <- testDataFrame_protein + 10^-7*as.data.frame(matrix(runif(n = NCol*NRow, min = -1, max = 1), ncol=NCol))
  # ###*****************************
  

  ###*****************************
  # Preperation of Data for PCA and PCoA
  # 1.) Correct batch effects
  # 2.) Remove rows (genes) with 0 sd from train  (PCA fails without this)
  # 3.) remove same genes from test set data
  # 4.) transpose the data frame
  
  # 1a. correcting batch effects mrna
  batchCorrDFs_mrna=batchCorrectSva(trainDataFrame_ = trainDataFrame_mrna,
                               meta_df_Train_ = meta_df_Train,
                               testDataFrame_ = testDataFrame_mrna,
                               dataNameDF_ = dataNameDF)
  
  trainDataFrame_mrna = batchCorrDFs_mrna$trainDataFrame
  testDataFrame_mrna = batchCorrDFs_mrna$testDataFrame
  
  
  # 1b. correcting batch effects protein
  batchCorrDFs_protein=batchCorrectSva(trainDataFrame_ = trainDataFrame_protein,
                                    meta_df_Train_ = meta_df_Train,
                                    testDataFrame_ = testDataFrame_protein,
                                    dataNameDF_ = dataNameDF)
  
  trainDataFrame_protein = batchCorrDFs_protein$trainDataFrame
  testDataFrame_protein = batchCorrDFs_protein$testDataFrame

  
  # 1c. combine results
  trainDataFrame_mrna %>% 
    tibble::rownames_to_column(df = ., var = "rowname")->trainDataFrame_mrna
  trainDataFrame_protein %>% 
    tibble::rownames_to_column(df = ., var = "rowname")->trainDataFrame_protein
  
  testDataFrame_mrna %>% 
    tibble::rownames_to_column(df = ., var = "rowname")->testDataFrame_mrna
  testDataFrame_protein %>% 
    tibble::rownames_to_column(df = ., var = "rowname")->testDataFrame_protein
  
  
  trainDataFrame = dplyr::bind_rows(trainDataFrame_mrna,trainDataFrame_protein)
  testDataFrame = dplyr::bind_rows(testDataFrame_mrna,testDataFrame_protein)
  
  trainDataFrame %>%
    tibble::column_to_rownames(df = ., var = "rowname")->trainDataFrame
  testDataFrame %>%
    tibble::column_to_rownames(df = ., var = "rowname")->testDataFrame
  

  
  # 2. remove rows
  zerosdList=which(apply(trainDataFrame,1,sd)==0)
  if(length(zerosdList)!=0){trainDataFrame=trainDataFrame[-zerosdList,]} # get rid of 0 sd data
  if(length(zerosdList)!=0){testDataFrame=testDataFrame[-zerosdList,]} # get rid of 0 sd data
  

  # 4. transpose data frames
  trainDataFrame=t(trainDataFrame)
  testDataFrame=t(testDataFrame)
  ###*****************************
  

  ###*****************************
  # Do PCA or PCoA
  if(dimReductionType=="PCA") 
  {mapped_DF_Objs=pca_analyze(train_set=trainDataFrame, 
                              train_condition=meta_df_Train, 
                              test_set=testDataFrame, 
                              test_condition=meta_df_Test)}
  
  mapped_train_DF=mapped_DF_Objs$train_set_PCs_comb
  mapped_test_DF=mapped_DF_Objs$test_set_PCs_comb
  # if(dimReductionType=="PCoA") 
  #   {mapped_DF=pcoa_analyze(mainDataFrame, inputMetaDf)}
  ###*****************************
  
  
  ###*****************************
  # Calculate Dimension Choice Automatically
  dimensionChoice=round(sqrt(nrow(meta_df_Train)))
  ###*****************************
  
  
  ###*****************************
  # Pick # of dimensions that needs to go into machine learning algorithm
  
  # generate the list of dimensions that will be selected from df by using dplyr::select
  selectList=c("conditionInvestigated",paste0("Axis.",seq(1,dimensionChoice))) 
  
  # selection of wanted dimensions with dplyr::select for "mapped_train_DF" and "mapped_test_DF"
  mapped_train_DF %>% 
    tibble::column_to_rownames(var = "dataSet")%>%
    dplyr::select_(.dots = selectList)->dim_reduced_train_DF 
  
  mapped_test_DF %>% 
    tibble::column_to_rownames(var = "dataSet")%>%
    dplyr::select_(.dots = selectList) %>%
    dplyr::group_by()%>%
    dplyr::select(-conditionInvestigated)->dim_reduced_test_DF
  ###*****************************
  
  dim_reduced_DF_obj<-list(dim_reduced_train_DF = dim_reduced_train_DF, 
                           dim_reduced_test_DF = dim_reduced_test_DF,
                           dimensionChoice = dimensionChoice)
  return(dim_reduced_DF_obj)
}