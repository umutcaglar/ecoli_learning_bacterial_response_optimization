# Batch Correction Function

# User defined functions
batchCorrectSva <- function(trainDataFrame_, meta_df_Train_, testDataFrame_, dataNameDF_)
{
  
  ###*****************************
  trainMod = model.matrix(~allConditionsTogether_Short,data=meta_df_Train_)
  trainMod0 = model.matrix(~1,data=meta_df_Train_)
  ###*****************************
  
  # the sva function calls La.svd which might cause frequent errors
  # solution is to rescale the input matrix slightly. For this I use try and catch instead of 
  # "trainSv = sva::sva(dat = as.matrix(trainDataFrame), mod = trainMod, mod0 = trainMod0, method = "irw")"
  # line
  resultTry<-try(expr = sva::sva(dat = as.matrix(trainDataFrame_), mod = trainMod, mod0 = trainMod0, method = "irw"))
  class(resultTry)
  counter03=1
  while(counter03<100 & class(resultTry)=="try-error")
  {
    print(counter03)
    resultTry<-try(expr = sva::sva(dat = as.matrix(trainDataFrame_*(1+0.0001*counter03)), mod = trainMod, mod0 = trainMod0, method = "irw"))
    counter03=counter03+1
  }
  trainSv = resultTry
  ###*****************************
  
  fsvaobj = fsva(dbdat = as.matrix(trainDataFrame_),
                 mod = trainMod, sv = trainSv,
                 newdat = as.matrix(testDataFrame_),
                 method = "exact")
  
  trainDataFrameCorr<-as.data.frame(fsvaobj$db)
  testDataFrameCorr<-as.data.frame(fsvaobj$new)
  
  batchCorrDFs=list(trainDataFrame = trainDataFrameCorr, testDataFrame = testDataFrameCorr)
  
  ###*****************************
  
  
  return(batchCorrDFs)
}
