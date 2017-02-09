# PCA and PCoA functions


###*****************************
#PCA Analyze Function
pca_analyze_wrong<-function(data_set, data_condition) {
  
  data_set.pca = prcomp((data_set), scale = TRUE, center=TRUE) # do pca

  data_set_PCs=as.data.frame(data_set.pca$x) # extract PCs
  colnames(data_set_PCs)<-gsub("^PC*","Axis.",colnames(data_set_PCs))
  
  data_set_PCs_comb=left_join(data_condition,cbind(dataSet=rownames(data_set_PCs),data_set_PCs)) # join conditions andPCs
  return(data_set_PCs_comb)
}
###*****************************


###*****************************
#PCoA Analyze Function

pcoa_analyze_wrong<-function(data_set, data_condition) {
  distObj=vegdist(data_set,method="manhattan")/1000
  data_set.pcoa=ape::pcoa(distObj) # Calculate PCoA
  data_set_scores=as.data.frame(data_set.pcoa$vectors) # Generating Score Matrices
  
  
  data_set_scores_comb=left_join(data_condition,cbind(dataSet=rownames(data_set_scores),data_set_scores)) # generate extended score matrices
  return(data_set_scores_comb)
}
###*****************************



###*****************************
#PCA Analyze Function
pca_analyze<-function(train_set, train_condition, test_set, test_condition) {
  
  #The line "train_set.pca = prcomp((train_set), scale = TRUE, center=TRUE)" might cause errors 
  # associated with "La.svd(x, nu, nv) : error code 1 from Lapack routine"
  # solution is to run a try function for that 
  train_set.pca = prcompTry(train_set_ = train_set, scale_ = TRUE, center_=TRUE) # do pca on training data 
  test_set.pred <- predict(train_set.pca, test_set) # rotate test data based on the result of pca on training data
  
  train_set_PCs=as.data.frame(train_set.pca$x) # extract PCs
  colnames(train_set_PCs)<-gsub("^PC*","Axis.",colnames(train_set_PCs))
  
  test_set_PCs=as.data.frame(test_set.pred) # Just renaming the file for consistency and turn it into DF
  colnames(test_set_PCs)<-gsub("^PC*","Axis.",colnames(test_set_PCs))
  
  train_condition=train_condition[,c("dataSet","conditionInvestigated")]
  test_condition=test_condition[,c("dataSet","conditionInvestigated")]
  train_set_PCs_comb=left_join(train_condition,cbind(dataSet=rownames(train_set_PCs),train_set_PCs)) # join conditions and PCs for training data
  test_set_PCs_comb=left_join(test_condition,cbind(dataSet=rownames(test_set_PCs),test_set_PCs)) # join conditions and PCs for test data

  data_set_PCs_comb=list(train_set_PCs_comb=train_set_PCs_comb,test_set_PCs_comb=test_set_PCs_comb)
  return(data_set_PCs_comb)
}
###*****************************


###*****************************
# prcompTry function
prcompTry<-function(train_set_, scale_ = TRUE, center_=TRUE)
{
  resultTry<-try(expr = prcomp(train_set_, scale = scale_, center=center_))
  class(resultTry)
  counter03=1
  while(counter03<100 & class(resultTry)=="try-error")
  {
    print(counter03)
    resultTry<-try(expr = prcomp(train_set_*(1+0.0001*counter03), scale = scale_, center=center_))
    counter03=counter03+1
  }
  return(resultTry)
}
###*****************************


###*****************************
#PCoA Analyze Function
pcoa_analyze<-function(...) 
  {stop("Rotation OBJ for PCoA is not ready yet")}
###*****************************