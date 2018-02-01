###*****************************
# F1 Function
f1 <- function (data, positive_ = "yes") 
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