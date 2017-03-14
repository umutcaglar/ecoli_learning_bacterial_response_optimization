# increase in performance after combining mRNA and proteins

###*****************************
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
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response_optimization/b_code_MLRN/"))} # mac computer
###*****************************


###*****************************
# REQUIRED LIBRARIES
# Data tracking
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
require("gtable") # for the "gtable_filter" function to seperately save the legend
require("grid") # For manipulating ggplot obj

# Text manipulation
require("stringr")
###*****************************


###*****************************
#Load Functions
source("../a_code_dataPreperation_RNA&Protein/replace_fun.R")	
###*****************************


###*****************************
# read data
winnerModels_int_protein = read.csv(file = "../b_results/model_performance_int_protein.csv")
winnerModels_int_mrna = read.csv(file = "../b_results/model_performance_int_mRNA.csv")
winnerModels_int_mrna_protein = read.csv(file = "../b_results/model_performance_int_mRNA_protein.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_int_mrna, 
                               winnerModels_int_protein,
                               winnerModels_int_mrna_protein)

winnerModels$analyzeName <- factor(winnerModels$analyzeName,
                                   levels = c("int_mRNA", "int_protein", "int_mRNA_protein"))

winnerModels$model <- factor(winnerModels$model,
                             levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# generate the increase in success figure
fig01<-ggplot(winnerModels, aes(x=model, y=performance, group=model))+
  facet_grid(.~analyzeName)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance))+
  theme_bw()+
  labs(title = "All conditions")

print(fig01)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/increasePerformanceAfterCombine_testAll.jpeg", plot = fig01, ncol = 2, nrow = 2)
###*****************************




















