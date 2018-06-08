# Combine to bae error graphs

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
# Require
require(tidyverse)
require(cowplot)
###*****************************

p1 <- ggdraw() + draw_image("../b_figures/fig_correct_prediction_mRNA.pdf", scale = 1)
p2 <- ggdraw() + draw_image("../b_figures/fig_correct_prediction_protein.pdf", scale = 1)
fig_error_cobined <- plot_grid(p1, p2, labels = "AUTO", ncol=2)


cowplot::save_plot(filename = "../b_figures/combined_correct_prediction.jpeg", 
                   plot = fig_error_cobined, ncol = 2, nrow = 1, dpi= 600)
cowplot::save_plot(filename = "../b_figures/combined_correct_prediction.pdf", 
                   plot = fig_error_cobined, ncol = 2, nrow = 1, dpi= 600)

print(fig_error_cobined)


# for generating this figure

# Run mRNA_analyze,.R and protein_analyze.R they will call mainAnalyzeCode.R (The figures will be generated there)
# then they are exported. This code call those 2 figures and combine them.