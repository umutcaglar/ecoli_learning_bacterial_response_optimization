# Try to understand ROC Curves

# This file is from https://heuristically.wordpress.com/2009/12/23/compare-performance-machine-learning-classifiers-r/

# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
seedNo=14159
set.seed(seedNo)
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response_optimization/b_code_MLRN/roc_curves/"))} # mac computer
###*****************************


###*****************************
# REQUIRED LIBRARIES
# Data tracking
require("magrittr")
require("DESeq2")
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
require("gtable") # for the "gtable_filter" function to seperately save the legend
require("grid") # For manipulating ggplot obj
require("ROCR")
require("pROC")

# Machine learning
require("e1071") # for svm

# The BreastCancer data set
require(mlbench)
require(party) # model using conditional inference trees
require(rpart) # Recursive partitioning for classification, regression and survival trees.
require(e1071)
require()
###*****************************


###*****************************
# load the data set
data(BreastCancer)
###*****************************


###*****************************
# Prepeare data

# omit NA
BreastCancer <- na.omit(BreastCancer) 

# remove the unique identifier, which is useless and would confuse the machine learning algorithms
BreastCancer$Id <- NULL

# partition the data set for 80% training and 20% evaluation (adapted from ?randomForest)
set.seed(2)
ind <- sample(2, nrow(BreastCancer), replace = TRUE, prob=c(0.8, 0.2))
###*****************************


###*****************************
# Method 1: Recursive partitioning on the training data set

# create model using recursive partitioning on the training data set
x.rp <- rpart(Class ~ ., data=BreastCancer[ind == 1,])
# predict classes for the evaluation data set
x.rp.pred <- predict(x.rp, type="class", newdata=BreastCancer[ind == 2,])
# score the evaluation data set (extract the probabilities)
x.rp.prob <- predict(x.rp, type="prob", newdata=BreastCancer[ind == 2,])

# To view the decision tree, uncomment this line.
par(mar=c(1,1,1,1))
plot(x.rp, main="Decision tree created using rpart")
###*****************************


###*****************************
# Random Forest
# create model using random forest and bagging ensemble using conditional inference trees
x.cf <- cforest(Class ~ ., data=BreastCancer[ind == 1,], control = cforest_unbiased(mtry = ncol(BreastCancer)-2))
x.cf.pred <- predict(x.cf, newdata=BreastCancer[ind == 2,])
x.cf.prob <-  1- unlist(treeresponse(x.cf, BreastCancer[ind == 2,]), use.names=F)[seq(1,nrow(BreastCancer[ind == 2,])*2,2)]
###*****************************


###*****************************
# create model using svm (support vector machine)
# svm requires tuning
x.svm.tune <- tune(svm, Class~., data = BreastCancer[ind == 1,],
                   ranges = list(gamma = 2^seq(from = -8,to = 8,length.out=15), 
                                 cost = 2^seq(from = -4,to = 4,length.out=15)),
                   tunecontrol = tune.control(sampling = "fix"))
# display the tuning results (in text format)
x.svm.tune
# If the tuning results are on the margin of the parameters (e.g., gamma = 2^-8), 
# then widen the parameters.
# I manually copied the cost and gamma from console messages above to parameters below.
x.svm <- svm(Class~., 
             data = BreastCancer[ind == 1,], 
             cost=x.svm.tune$best.model$cost, 
             gamma=x.svm.tune$best.model$gamma, 
             probability = TRUE)
x.svm.prob <- predict(x.svm, type="prob", newdata=BreastCancer[ind == 2,], probability = TRUE)
###*****************************




## plot ROC curves to compare the performance of the individual classifiers
###*****************************

# Output the plot to a PNG file for display on web.  To draw to the screen, 
# comment this line out.
png(filename="roc_curve_2_models.png", width=700, height=700)


# cforest
x.cf.prob.rocr <- ROCR::prediction(x.cf.prob, BreastCancer[ind == 2,'Class'])
x.cf.perf <- ROCR::performance(x.cf.prob.rocr, "tpr","fpr")
plot(x.cf.perf, col=4, main="ROC curves comparing classification performance of five machine learning models")

# Draw a legend.
legend(0.6, 0.6, c('cforest','svm'), 2:6)

# svm
x.svm.prob.rocr <- ROCR::prediction(attr(x.svm.prob, "probabilities")[,2], BreastCancer[ind == 2,'Class'])
x.svm.perf <- ROCR::performance(x.svm.prob.rocr, "tpr","fpr")
plot(x.svm.perf, col=6, add=TRUE)


# Close and save the PNG file.
dev.off()
###*****************************

