# change between exp stationary

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
winnerModels_mrna_exp_carbon = read.csv(file = "../b_results/model_performance_mRNA_exp_carbon.csv")
winnerModels_mrna_sta_carbon = read.csv(file = "../b_results/model_performance_mRNA_sta_carbon.csv")
winnerModels_mrna_exp_Mg = read.csv(file = "../b_results/model_performance_mRNA_exp_Mg.csv")
winnerModels_mrna_sta_Mg = read.csv(file = "../b_results/model_performance_mRNA_sta_Mg.csv")
winnerModels_mrna_exp_Na = read.csv(file = "../b_results/model_performance_mRNA_exp_Na.csv")
winnerModels_mrna_sta_Na = read.csv(file = "../b_results/model_performance_mRNA_sta_Na.csv")

winnerModels_protein_exp_carbon = read.csv(file = "../b_results/model_performance_protein_exp_carbon.csv")
winnerModels_protein_sta_carbon = read.csv(file = "../b_results/model_performance_protein_sta_carbon.csv")
winnerModels_protein_exp_Mg = read.csv(file = "../b_results/model_performance_protein_exp_Mg.csv")
winnerModels_protein_sta_Mg = read.csv(file = "../b_results/model_performance_protein_sta_Mg.csv")
winnerModels_protein_exp_Na = read.csv(file = "../b_results/model_performance_protein_exp_Na.csv")
winnerModels_protein_sta_Na = read.csv(file = "../b_results/model_performance_protein_sta_Na.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_mrna_exp_carbon, winnerModels_mrna_exp_Mg,  winnerModels_mrna_exp_Na, 
                               winnerModels_mrna_sta_carbon, winnerModels_mrna_sta_Mg,  winnerModels_mrna_sta_Na, 
                               winnerModels_protein_exp_carbon, winnerModels_protein_exp_Mg, winnerModels_protein_exp_Na, 
                               winnerModels_protein_sta_carbon, winnerModels_protein_sta_Mg, winnerModels_protein_sta_Na)

winnerModels %>%
  dplyr::group_by(experiment)%>%
  dplyr::mutate(phase=ifelse(grepl(pattern = "*Exp*", x = experiment, ignore.case = TRUE), "Exp", "Sta"))%>%
  dplyr::mutate(pick_data=ifelse(grepl(pattern = "*mrna*", x = experiment, ignore.case = TRUE), "mRNA", "Protein"))%>%
  dplyr::mutate(testFor=ifelse(grepl(pattern = "*_carbon*", x = experiment, ignore.case = TRUE), "carbon", "q"),
                testFor=ifelse(grepl(pattern = "*_Mg*", x = experiment, ignore.case = TRUE), "Mg", testFor),
                testFor=ifelse(grepl(pattern = "*_Na*", x = experiment, ignore.case = TRUE), "Na", testFor))->winnerModels

winnerModels%>%
  dplyr::group_by(phase, pick_data, model, testFor)%>%
  dplyr::summarise(meanPerformance= mean(performance))->winnerModelsSummary


# winnerModels$experiment <- factor(winnerModels$experiment,
#                                   levels = c("int_mRNA", "int_protein", "int_mRNA_protein"))
# 
# winnerModels$model <- factor(winnerModels$model,
#                              levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# generate the increase in success figure
fig01<-ggplot(winnerModelsSummary, aes(x=phase, y=meanPerformance, group=model, colour=model))+
  facet_grid(testFor~ pick_data)+
  geom_point(aes(colour=model), size=1.5)+
  geom_line(size=1)+
  theme_bw()+
  labs(title = "All conditions")

print(fig01)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/changePerformanceExpSta_testAll.jpeg", plot = fig01, ncol = 1, nrow = 1.2)
###*****************************




















