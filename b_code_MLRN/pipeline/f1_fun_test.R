# Testing f1 score

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list= ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************


###*****************************
# add libraries
require(tidyverse)
require(cowplot)
###*****************************


###*****************************
# Source Functions
source(file = "f1_fun.R")
###*****************************


###*****************************
# generate data
generate_fake <- function(TP, FP, FN, TN)
{
  df_TP <- data.frame(obs = rep(x = 1, times = TP), pred = rep(x = 1, times = TP))
  df_FP <- data.frame(obs = rep(x = 0, times = FP), pred = rep(x = 1, times = FP))
  df_FN <- data.frame(obs = rep(x = 1, times = FN), pred = rep(x = 0, times = FN))
  df_TN <- data.frame(obs = rep(x = 0, times = TN), pred = rep(x = 0, times = TN))
  
  df <- bind_rows(df_TP, df_FP, df_FN, df_TN)
  df[sample(nrow(df)),]
  return(df)
}
###*****************************


###*****************************
# with sparse data

df <- generate_fake(TP = 10, FP = 15, FN = 20, TN = 25000)
f1(data = df, positive_ = "1", verbal = FALSE) 

df <- generate_fake(TP = 10, FP = 15, FN = 20, TN = 0)
f1(data = df, positive_ = "0", verbal = FALSE) 
f1_multi_cond(data = df)
###*****************************


###*****************************
# with algorithm that always say no

df <- generate_fake(TP = 0, FP = 0, FN = 20, TN = 24980)
f1(data = df, positive_ = "0", verbal = FALSE) 
f1_multi_cond(data = df)
###*****************************


###*****************************
# Compare with old
df <- generate_fake(TP = 20, FP = 30, FN = 40, TN = 24980)
f1_multi_cond(data = df)
1-F1ScoreErr(y = df$obs, prediction = df$pred)
###*****************************