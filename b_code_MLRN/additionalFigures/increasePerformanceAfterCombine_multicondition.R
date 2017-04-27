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
winnerModels_int_protein_carbon = read.csv(file = "../b_results/model_performance_int_protein_carbon.csv")
winnerModels_int_protein_Mg = read.csv(file = "../b_results/model_performance_int_protein_Mg.csv")
winnerModels_int_protein_Na = read.csv(file = "../b_results/model_performance_int_protein_Na.csv")
winnerModels_int_protein_phase = read.csv(file = "../b_results/model_performance_int_protein_phase.csv")

winnerModels_int_mrna_carbon = read.csv(file = "../b_results/model_performance_int_mRNA_carbon.csv")
winnerModels_int_mrna_Mg = read.csv(file = "../b_results/model_performance_int_mRNA_Mg.csv")
winnerModels_int_mrna_Na = read.csv(file = "../b_results/model_performance_int_mRNA_Na.csv")
winnerModels_int_mrna_phase = read.csv(file = "../b_results/model_performance_int_mRNA_phase.csv")

winnerModels_int_mrna_protein_carbon = read.csv(file = "../b_results/model_performance_int_mRNA_protein_carbon.csv")
winnerModels_int_mrna_protein_Mg = read.csv(file = "../b_results/model_performance_int_mRNA_protein_Mg.csv")
winnerModels_int_mrna_protein_Na = read.csv(file = "../b_results/model_performance_int_mRNA_protein_Na.csv")
winnerModels_int_mrna_protein_phase = read.csv(file = "../b_results/model_performance_int_mRNA_protein_phase.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_int_mrna_carbon,
                               winnerModels_int_mrna_Mg,
                               winnerModels_int_mrna_Na,
                               winnerModels_int_mrna_phase,
                               
                               winnerModels_int_protein_carbon,
                               winnerModels_int_protein_Mg,
                               winnerModels_int_protein_Na,
                               winnerModels_int_protein_phase,
                               
                               winnerModels_int_mrna_protein_carbon,
                               winnerModels_int_mrna_protein_Mg,
                               winnerModels_int_mrna_protein_Na,
                               winnerModels_int_mrna_protein_phase)

winnerModels %>%dplyr::mutate(testFor=analyzeName)->winnerModels
winnerModels$testFor %>%
  gsub(pattern= "int_", replacement = "", x=.) %>%
  gsub(pattern= "mRNA_", replacement = "", x=.) %>%
  gsub(pattern= "protein_", replacement = "", x=.) -> winnerModels$testFor

winnerModels %>%dplyr::mutate(dataSource=analyzeName)->winnerModels
winnerModels$dataSource %>%
  gsub(pattern= "_phase", replacement = "", x=.) %>%
  gsub(pattern= "_carbon", replacement = "", x=.) %>%
  gsub(pattern= "_Mg", replacement = "", x=.) %>%
  gsub(pattern= "_Na", replacement = "", x=.) %>%
  gsub(pattern= "_", replacement = " ", x=.) -> winnerModels$dataSource

winnerModels$testFor <- factor(winnerModels$testFor,
                                  levels = c("carbon", "Mg", "Na", "phase"))

factor(winnerModels$dataSource)
winnerModels$dataSource <- factor(winnerModels$dataSource,
                                   levels = c("int mRNA", "int protein", "int mRNA protein"))

winnerModels$model <- factor(winnerModels$model,
                             levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# generate the increase in success figure
fig01<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
  facet_grid(testFor~dataSource)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw()+
  labs(title = "All conditions") + xlab("Model") + ylab("F1 performance on test data")

print(fig01)


fig02<-ggplot(winnerModels, aes(x=dataSource, y=meanPerformance_test, group=model, colour=model))+
  facet_grid(.~testFor)+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title = "All conditions") + xlab("Model") + ylab("F1 performance on test data")
print(fig02)


winnerModels%>%
  dplyr::group_by(model, testFor, dataSource)%>%
  dplyr::summarise(meanPerformance_test=unique(meanPerformance_test))%>%
  dplyr::group_by(model,dataSource)%>%
  dplyr::summarise(meanPerformance_test=mean(meanPerformance_test))->winnerModels_short

fig03<-ggplot(winnerModels_short, aes(x=dataSource, y=meanPerformance_test, group=model, colour=model))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title = "All conditions") + xlab("Model") + ylab("F1 performance on test data")
print(fig03)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/increasePerformanceAfterCombine_multicondition_testOne.jpeg", 
                   plot = fig01, ncol = 2, nrow = 2)
cowplot::save_plot(filename = "../b_figures/increasePerformanceAfterCombine_multicondition_testOne_ppt.jpeg", 
                   plot = fig01, ncol = 2, nrow = 1.4)
###*****************************

