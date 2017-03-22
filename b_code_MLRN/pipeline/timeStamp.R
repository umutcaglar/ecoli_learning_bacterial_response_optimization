print("a")

timeStampVector=dataNameDF[2:length(dataNameDF)]
colnames(timeStampVector)<-gsub(pattern = "objectName.",
                                replacement = "",
                                colnames(timeStampVector))
timeStampVector$numRepeatsFor_TestTrainSubset_Choice=numRepeatsFor_TestTrainSubset_Choice
timeStampVector$percentTest=percentTest
timeStampVector$testConditions=paste0(testConditions,collapse = "_")
timeStampVector$dimReductionType=dimReductionType
timeStampVector$batchCorrectionMethod=batchCorrectionMethod
timeStampVector$classWeightInputType=classWeightInputType
timeStampVector$similarDataClassifierForBatch=paste0(similarDataClassifierForBatch,collapse = "_")
timeStampVector$type_svmChoice=type_svmChoice

# SVM related parameters
timeStampVector$crossValue=crossValue
timeStampVector$nrepeatValue=nrepeatValue
timeStampVector$samplingValue=samplingValue

# Parameter Span SVM
timeStampVector$powerRangeGammaLow=powerRangeGammaLow
timeStampVector$powerRangeGammaHigh=powerRangeGammaHigh
timeStampVector$powerRangeCostLow=powerRangeCostLow
timeStampVector$powerRangeCostHigh=powerRangeCostHigh
timeStampVector$ndivisionCost=ndivisionCost
timeStampVector$ndivisionGamma=ndivisionGamma
timeStampVector$kernelList=paste0(kernelList,collapse = "_")

# RF Related parameters
timeStampVector$ntreelistRF=paste(ntreelistRF,collapse = "_")
timeStampVector$nodesizelistRF=paste(nodesizelistRF,collapse = "_")
timeStampVector$mtrylistRF=paste(mtrylistRF,collapse = "_")

# combined set related variables
timeStampVector$batchCorrectionType=batchCorrectionType

# arrange tuning
timeStampVector$tuning = tuning

# parallel processing
timeStampVector$parallel_com = parallel_com

# ncore
timeStampVector$ncore=getDoParWorkers()

#seed
timeStampVector$seedNo=seedNo

# file Name
options(digits.secs=6)
time1<-Sys.time();
print(time1);
time2<-gsub(pattern = "*.*\\.",replacement = "", x = time1);
time3<-paste0(format(time1, "%Y_%m_%d_%H_%M_%S"),"_",time2)
fileName=paste0("RunNo_",time3)

timeStampVector$fileName=fileName
