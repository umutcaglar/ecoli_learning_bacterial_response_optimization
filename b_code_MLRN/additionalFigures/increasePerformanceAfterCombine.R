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
fig01<-ggplot(winnerModels, aes(x=model, y=performance_test, group=model))+
  facet_grid(.~analyzeName)+
  geom_violin(aes(fill=model, color=model))+
  geom_point(aes(x=model, y=meanPerformance_test))+
  theme_bw()+
  labs(title = "All conditions")

print(fig01)
###*****************************


###*****************************
# Save figure
cowplot::save_plot(filename = "../b_figures/increasePerformanceAfterCombine_testAll.jpeg", plot = fig01, ncol = 2, nrow = 2)
###*****************************


###*****************************
# Paired T Test Results
modelVector=levels(winnerModels$model)
analyzeVector=levels(winnerModels$analyzeName)

for(counter01 in 1: length(modelVector))
{
  winnerModels %>% 
    dplyr::filter(model==modelVector[counter01])->winnerModelSub
  
  winnerModelSub %>%
    dplyr::filter(analyzeName=="int_mRNA") %>%
    .$performance_test->v_mrna
  
  winnerModelSub %>%
    dplyr::filter(analyzeName=="int_protein") %>%
    .$performance_test->v_protein
  
  winnerModelSub %>%
    dplyr::filter(analyzeName=="int_mRNA_protein") %>%
    .$performance_test->v_mRNA_protein
  
  VmrnaVSprotein=c(model=modelVector[counter01],
                   comparison="mrna VS protein",
                   p_value=t.test(x= v_mrna, y= v_protein, paired = TRUE)$p.value)
  
  VmrnaproteinVSmrna=c(model=modelVector[counter01],
                   comparison="mrna&protein VS mrna",
                   p_value=t.test(x= v_mRNA_protein, y= v_mrna, paired = TRUE)$p.value)
  
  VmrnaproteinVSprotein=c(model=modelVector[counter01],
                   comparison="mrna&protein VS protein",
                   p_value=t.test(x= v_mRNA_protein, y= v_protein, paired = TRUE)$p.value)
  
  temp_p_valueTable = t(data.frame(VmrnaVSprotein, 
                                VmrnaproteinVSmrna, 
                                VmrnaproteinVSprotein))
  
  if(counter01==1){p_valueTable = temp_p_valueTable}
  if(counter01!=1){p_valueTable = rbind(p_valueTable,temp_p_valueTable)}
}

p_valueDF=data.frame(p_valueTable)
rownames(p_valueDF)<-NULL
p_valueDF %>% 
  .$p_value %>% as.vector(.) %>% p.adjust(p=.,method="fdr") %>%
  as.data.frame(.) ->padj

colnames(padj)<-"padj"

dplyr::bind_cols(p_valueDF, padj)->p_valueDF

write.csv(x = p_valueDF, file = "../b_results/p_values_increasePerformanceAfterCombine.csv")
###*****************************

















