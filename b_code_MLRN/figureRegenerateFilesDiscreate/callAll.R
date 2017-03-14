# Call all

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
# main results
source("figureRegenerateFilesDiscreate/mRNA_analyze.R")
source("figureRegenerateFilesDiscreate/protein_analyze.R")

# intersection results
source("figureRegenerateFilesDiscreate/int_mRNA_analyze.R")
source("figureRegenerateFilesDiscreate/int_protein_analyze.R")
source("figureRegenerateFilesDiscreate/int_mRNA_protein_analyze.R")

# phase difference
source("figureRegenerateFilesDiscreate/mRNA_exp_analyze.R")
source("figureRegenerateFilesDiscreate/mRNA_sta_analyze.R")
source("figureRegenerateFilesDiscreate/protein_exp_analyze.R")
source("figureRegenerateFilesDiscreate/protein_sta_analyze.R")
###*****************************



