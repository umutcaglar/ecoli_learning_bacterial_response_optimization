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
fig01<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
  facet_grid(.~analyzeName)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw(base_size=16)+
  #labs(title = "All conditions") + 
  xlab("Model") + ylab("F1 Performance on Test Data")

print(fig01)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/compare_mRNA_protein_models.pdf", 
                   plot = fig01, ncol = 2, nrow = 1.3)
cowplot::save_plot(filename = "../b_figures/compare_mRNA_protein_models_ppt.pdf", 
                   plot = fig01, ncol = 2, nrow = 1.4)
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
combinedSquarePlots<-cowplot::ggdraw() +
  cowplot::draw_plot(plot=figComb_mrna, x = 0, y = 6/11, width = 1, height = 5/11) +
  cowplot::draw_plot(plot=figComb_protein, x = 0, y = 1/11, width = 1, height = 5/11) +
  cowplot::draw_plot(plot=color_legend, x = 0, y = 0, width = 1, height = 1/11) +
  cowplot::draw_plot_label(c("A", "", "B"), c(0, 0, 0), c(1, 1/11, 6/11), size = 15)

print(combinedSquarePlots)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/combined_mRNA_protein_square.jpeg",
                   plot = combinedSquarePlots, ncol = 2, nrow = 2.8)
###*****************************


###*****************************
# Individual condition performance
load(file = "../b_figures/fig_correlation_mRNA.RDA")
assign(x = "fig_correlation_mRNA", value = fig_correlation)
load(file = "../b_figures/fig_correlation_protein.RDA")
assign(x = "fig_correlation_protein", value = fig_correlation)

combinedCorrelation = cowplot::plot_grid(fig_correlation_mRNA,fig_correlation_protein, 
                                         ncol = 1,nrow=2,scale = .95,labels = c("A","B"))


scale_val = 1.5
cowplot::save_plot(filename = "../b_figures/mRNA_correlation.jpeg",
                   plot = fig_correlation_mRNA, ncol = 1.3*scale_val, nrow = 1*scale_val)
cowplot::save_plot(filename = "../b_figures/protein_correlation.jpeg",
                   plot = fig_correlation_protein, ncol = 1.3*scale_val, nrow = 1*scale_val)

cowplot::save_plot(filename = "../b_figures/combined_mRNA_protein_correlation.jpeg",
                   plot = combinedCorrelation, ncol = 2*1.3, nrow = 2.8*1.3)
###*****************************














