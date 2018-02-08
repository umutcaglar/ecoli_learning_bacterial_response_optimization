###*****************************
# F1 Function
f1 <- function (data, positive_ = "yes", verbal = FALSE) 
{
  pred_n = as.numeric(data$pred==positive_)
  obs_n = as.numeric(data$obs==positive_)
  
  #pred_n[is.na(pred_n)] <- 0
  #obs_n[is.na(obs_n)] <- 0
  
  TP = as.numeric(pred_n == 1 & obs_n == 1)
  FP = as.numeric(pred_n == 1 & obs_n == 0)
  FN = as.numeric(pred_n == 0 & obs_n == 1)
  TN = as.numeric(pred_n == 0 & obs_n == 0)
  
  TPs = sum(TP)
  FPs = sum(FP)
  FNs = sum(FN)
  TNs = sum(TN)
  count = length(TP)
  
  if(verbal)
  {
    print(paste0("TPs: ", TPs))
    print(paste0("FPs: ", FPs))
    print(paste0("FNs: ", FNs))
    print(paste0("TNs: ", TNs))
  }
  
  if(count == TNs)
  {
    f1_val = NA
  }else{
    f1_val = 2*TPs/(2*TPs + FPs + FNs)
  }
  
  #f1_val = 1 - f1_val 
  names(f1_val) <- c("F1")
  f1_val
} 
###*****************************



###*****************************
# F1 Multiconditional Function
f1_multi_cond <- function (data)
{
  class_list <- sort(unique(c(as.vector(data[["obs"]]), 
                              as.vector(data[["pred"]]))))
  
  f1_list = c()
  for(counter01 in 1: length(class_list))
  {
    positive_ <- class_list[counter01]
    f1_list[counter01] <- f1(data = data , positive_ = positive_)
  }
  
  print(f1_list)
  f1_score = mean(f1_list, na.rm = TRUE)
  return(f1_score)
}


###*****************************



# Alternatives DO NOT USE

F1ScoreErr<-function(y,prediction)
{
  beta=1
  y=make.names(y)
  prediction=make.names(prediction)
  a=as.vector(y); b=as.vector(prediction)
  
  inputs=sort(unique(c(a,b)))
  
  TP=c();
  FP=c();
  FN=c();
  F1=c();
  
  for (counter04 in 1:length(inputs))
  {
    testFor=inputs[counter04]
    
    sum(a==testFor & b==testFor)->TP[counter04]
    sum(a!=testFor & b==testFor)->FP[counter04]
    sum(a==testFor & b!=testFor)->FN[counter04]
    
    F1[counter04]=(2*TP[counter04])/(2*TP[counter04]+FP[counter04]+FN[counter04])
    if(0==2*TP[counter04]+FP[counter04]+FN[counter04]){F1[counter04]==0}
  }
  
  # print(paste0("TP: ", paste0(TP, collapse = " ")));
  # print(paste0("FP: ", paste0(FP, collapse = " ")));
  # print(paste0("FN: ", paste0(FN, collapse = " ")));
  # print(paste0("F1: ", paste0(F1, collapse = " ")));
  
  # Controls
  sum_TP = sum(TP, na.rm =T)
  sum_FP = sum(FP, na.rm =T)
  sum_FN = sum(FN, na.rm =T)
  if(sum_FP!=sum_FN){browser()}
  if(length(y)!=length(prediction)){browser()}
  if(length(y)!=sum_TP + sum_FP){browser()}
  
  F1_err=1-mean(F1, na.rm =T)
  # print(paste0("F1_err: ", F1_err));
  
  return(F1_err)
}






F1ScoreErr1<-function(y,prediction)
{
  beta=1
  y=make.names(y)
  prediction=make.names(prediction)
  a=as.vector(y); b=as.vector(prediction)
  
  inputs=sort(unique(c(a,b)))
  #print(paste0("inputs: ", paste0(inputs, collapse = " ")))
  
  TP=c();
  FP=c();
  FN=c();
  for (counter04 in 1:length(inputs))
  {
    testFor=inputs[counter04]
    
    sum((a==testFor) & (b==testFor))->TP[counter04]
    sum((!a==testFor) & (b==testFor))->FP[counter04]
    sum((a==testFor) & (!b==testFor))->FN[counter04]
  }
  
  #print(paste0("TP: ", paste0(TP, collapse = " ")));
  #print(paste0("FP: ", paste0(FP, collapse = " ")));
  #print(paste0("FN: ", paste0(FN, collapse = " ")));
  
  # Controls
  sum_TP = sum(TP, na.rm =T)
  sum_FP = sum(FP, na.rm =T)
  sum_FN = sum(FN, na.rm =T)
  if(sum_FP!=sum_FN){browser()}
  if(length(y)!=length(prediction)){browser()}
  if(length(y)!=sum_TP + sum_FP){browser()}
  
  #print(paste0("sum FP: ", sum_FP))
  #print(paste0("sum FN: ", sum_FN))
  
  PRE_vec = TP/(TP+FP);
  REC_vec = TP/(TP+FN);
  
  #print(paste0("PRE_vec: ", paste0(PRE_vec, collapse = " ")))
  #print(paste0("REC_vec: ", paste0(REC_vec, collapse = " ")))
  
  PRE_m = mean(PRE_vec, na.rm=T);
  REC_m = mean(REC_vec, na.rm=T);
  
  #print(paste0("PRE_m: ",PRE_m))
  #print(paste0("REC_m: ",REC_m))
  
  Fscore_m = ((beta^2+1)*PRE_m*REC_m)/((beta^2)*PRE_m+REC_m);
  if(PRE_m+REC_m==0){Fscore_m = 0}
  #print(paste0("Fscore_m1: ",Fscore_m))
  
  F1_err=1-Fscore_m
  return(F1_err)
}