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
winnerModels_protein = read.csv(file = "../b_results/model_performance_mRNA.csv")
winnerModels_mrna = read.csv(file = "../b_results/model_performance_protein.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_mrna, 
                               winnerModels_protein)

winnerModels$analyzeName <- factor(winnerModels$analyzeName,
                                   levels = c("mRNA", "protein"))

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
cowplot::save_plot(filename = "../b_figures/compare_mRNA_protein_models.jpeg", plot = fig01, ncol = 2, nrow = 2)
###*****************************


###*****************************
# load square plots
load("../b_figures/fig_obj_mRNA.Rda")
figComb_mrna<-figComb
figComb_wL_mrna<-figComb_wL

load("../b_figures/fig_obj_protein.Rda")
figComb_protein<-figComb
figComb_wL_protein<-figComb_wL
###*****************************


###*****************************
# combine figures
combinedSquarePlots<-ggdraw() +
  draw_plot(plot=figComb_mrna, x = 0, y = 6/11, width = 1, height = 5/11) +
  draw_plot(plot=figComb_protein, x = 0, y = 1/11, width = 1, height = 5/11) +
  draw_plot(plot=color_legend, x = 0, y = 0, width = 1, height = 1/11) +
  draw_plot_label(c("A", "", "B"), c(0, 0, 0), c(1, 1/11, 6/11), size = 15)

print(combinedSquarePlots)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/combined_mRNA_protein_square.jpeg", 
                   plot = combinedSquarePlots, ncol = 2, nrow = 2.8)
###*****************************














