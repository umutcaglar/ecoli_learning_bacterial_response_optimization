# dintinct test condition performances of mRNA and protein 

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
winnerModels_mRNA_carbon = read.csv(file = "../b_results/model_performance_mRNA_carbon.csv")
winnerModels_mRNA_growth = read.csv(file = "../b_results/model_performance_mRNA_growth.csv")
winnerModels_mRNA_Mg = read.csv(file = "../b_results/model_performance_mRNA_Mg.csv")
winnerModels_mRNA_Na = read.csv(file = "../b_results/model_performance_mRNA_Na.csv")
winnerModels_protein_carbon = read.csv(file = "../b_results/model_performance_protein_carbon.csv")
winnerModels_protein_growth = read.csv(file = "../b_results/model_performance_protein_growth.csv")
winnerModels_protein_Mg = read.csv(file = "../b_results/model_performance_protein_Mg.csv")
winnerModels_protein_Na = read.csv(file = "../b_results/model_performance_protein_Na.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_mRNA_carbon,
                               winnerModels_mRNA_growth,
                               winnerModels_mRNA_Mg,
                               winnerModels_mRNA_Na,
                               winnerModels_protein_carbon,
                               winnerModels_protein_growth,
                               winnerModels_protein_Mg,
                               winnerModels_protein_Na)

winnerModels%>%
  tidyr::separate(analyzeName, c("pick_data", "tested_for"), "_")->winnerModels

winnerModels$analyzeName <- factor(winnerModels$tested_for,
                                   levels = c("carbon", "growth", "Mg", "Na"))

winnerModels$analyzeName <- factor(winnerModels$pick_data,
                                   levels = c("mRNA", "protein"))

winnerModels$model <- factor(winnerModels$model,
                             levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# generate the increase in success figure
fig01<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
  facet_grid(pick_data ~ tested_for)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw()+
  labs(title = "All conditions")

print(fig01)

# fig02<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
#   facet_grid(.~analyzeName)+
#   geom_violin(aes(fill=model, color=model))+
#   geom_point(aes(x=model, y=meanPerformance_test))+
#   theme_bw()+
#   labs(title = "All conditions")
# 
# print(fig02)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/distinctTests_mRNA_Protein.jpeg", 
                   plot = fig01, ncol = 2, nrow = 2)

cowplot::save_plot(filename = "../b_figures/distinctTests_mRNA_Protein_ppt.jpeg", 
                   plot = fig01, ncol = 2, nrow = 1.4)
###*****************************



# load square plots
###*****************************

# mRNA
load("../b_figures/fig_obj_mRNA_carbon.Rda")
figComb_mrna_carbon<-figComb

load("../b_figures/fig_obj_mRNA_growth.Rda")
figComb_mrna_growth<-figComb

load("../b_figures/fig_obj_mRNA_Mg.Rda")
figComb_mrna_Mg<-figComb

load("../b_figures/fig_obj_mRNA_Na.Rda")
figComb_mrna_Na<-figComb

# Protein
load("../b_figures/fig_obj_protein_carbon.Rda")
figComb_protein_carbon<-figComb

load("../b_figures/fig_obj_protein_growth.Rda")
figComb_protein_growth<-figComb

load("../b_figures/fig_obj_protein_Mg.Rda")
figComb_protein_Mg<-figComb

load("../b_figures/fig_obj_protein_Na.Rda")
figComb_protein_Na<-figComb
###*****************************


###*****************************
# Combine Plots

fig02a<-cowplot::plot_grid(figComb_mrna_carbon, figComb_mrna_growth, figComb_mrna_Mg, figComb_mrna_Na,
                   figComb_protein_carbon, figComb_protein_growth, figComb_protein_Mg, figComb_protein_Na, 
                   nrow = 2, ncol = 4, scale = .9, labels = c("A","B","C","D","E","F","G","I"))

print(fig02a)


fig02b<-cowplot::plot_grid(figComb_mrna_carbon, figComb_protein_carbon,
                           figComb_mrna_growth, figComb_protein_growth,
                           figComb_mrna_Mg, figComb_protein_Mg,
                           figComb_mrna_Na, figComb_protein_Na, 
                           nrow = 4, ncol = 2, scale = .9, labels = c("A","B","C","D","E","F","G","I"))

print(fig02b)


cowplot::save_plot(filename = "../b_figures/distinctTestsConfMatrix_mRNA_Protein_ppt.jpeg", 
                   plot = fig02a, ncol = 4, nrow = 2)

cowplot::save_plot(filename = "../b_figures/distinctTestsConfMatrix_mRNA_Protein.jpeg", 
                   plot = fig02b, ncol = 2, nrow = 4)

###*****************************













