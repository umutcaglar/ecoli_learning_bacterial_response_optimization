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
winnerModels_mrna_exp = read.csv(file = "../b_results/model_performance_mRNA_exp.csv")
winnerModels_mrna_sta = read.csv(file = "../b_results/model_performance_mRNA_sta.csv")

winnerModels_protein_exp = read.csv(file = "../b_results/model_performance_protein_exp.csv")
winnerModels_protein_sta = read.csv(file = "../b_results/model_performance_protein_sta.csv")
###*****************************


###*****************************
# combine data
winnerModels<-dplyr::bind_rows(winnerModels_mrna_exp,
                               winnerModels_mrna_sta,
                               winnerModels_protein_exp,
                               winnerModels_protein_sta)

winnerModels %>%
  dplyr::group_by(analyzeName)%>%
  dplyr::mutate(phase=ifelse(grepl(pattern = "*Exp*", x = analyzeName, ignore.case = TRUE), "Exp", "Sta"))%>%
  dplyr::mutate(pick_data=ifelse(grepl(pattern = "*mrna*", x = analyzeName, ignore.case = TRUE), "mRNA", "Protein"))->winnerModels

winnerModels%>%
  dplyr::group_by(phase, pick_data, model)%>%
  dplyr::summarise(meanPerformance= mean(performance),
                   meanPerformance_test=mean(performance_test))->winnerModelsSummary


# winnerModels$experiment <- factor(winnerModels$experiment,
#                                   levels = c("int_mRNA", "int_protein", "int_mRNA_protein"))
#
# winnerModels$model <- factor(winnerModels$model,
#                              levels = c("radial", "sigmoid", "linear", "RF"))
###*****************************


###*****************************
# generate the increase in success figure
fig01<-ggplot(winnerModelsSummary, aes(x=phase, y=meanPerformance_test, group=model, colour=model))+
  facet_grid(. ~ pick_data)+
  xlab("Phase")+ ylab("Mean F1 performance on test data")+
  geom_point(aes(colour=model), size=1.5)+
  geom_line(size=1)+
  theme_bw()

print(fig01)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/changePerformanceExpSta_testAll.jpeg", plot = fig01, ncol = 1, nrow = 1.2)
###*****************************




















