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
require("pROC")

# Machine learning
require("e1071") # for svm
require("randomForest")

# The BreastCancer data set
###*****************************


###*****************************
# load the data set
mainData <- data(iris)
###*****************************


###*****************************
# Train Random Forest
rf = randomForest(Species~., data = iris, ntree = 100)
###*****************************


###*****************************
# Make predictions
predictions <- as.numeric(predict(rf, iris, type = 'response'))
pROC::plot.roc((multiclass.roc(iris$Species, predictions)))

roc_rose <- plot(roc(hacide.test$cls, pred_rose[,2]), print.auc = TRUE, col = "blue")
###*****************************

