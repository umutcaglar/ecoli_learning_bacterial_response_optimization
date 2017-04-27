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

# call full mrna & protein
source("starterFilesDiscreate/code_mrna.R")
source("starterFilesDiscreate/code_protein.R")

# Call intersections
source("starterFilesDiscreate/code_int_mrna.R")
source("starterFilesDiscreate/code_int_protein.R")
source("starterFilesDiscreate/code_int_mrna_protein.R")

# call phases
source("starterFilesDiscreate/code_mrna_exp.R")
source("starterFilesDiscreate/code_mrna_sta.R")
source("starterFilesDiscreate/code_protein_exp.R")
source("starterFilesDiscreate/code_protein_sta.R")

# call mrna subs
source("starterFilesDiscreate/code_mrna_carbon.R")
source("starterFilesDiscreate/code_mrna_phase.R")
source("starterFilesDiscreate/code_mrna_Mg.R")
source("starterFilesDiscreate/code_mrna_Na.R")

# call for protein subs
source("starterFilesDiscreate/code_protein_carbon.R")
source("starterFilesDiscreate/code_protein_phase.R")
source("starterFilesDiscreate/code_protein_Mg.R")
source("starterFilesDiscreate/code_protein_Na.R")

# Call intersected data combinations
source("starterFilesDiscreate/code_int_mrna_carbon.R")
source("starterFilesDiscreate/code_int_mrna_Mg.R")
source("starterFilesDiscreate/code_int_mrna_Na.R")
source("starterFilesDiscreate/code_int_mrna_phase.R")

source("starterFilesDiscreate/code_int_protein_carbon.R")
source("starterFilesDiscreate/code_int_protein_Mg.R")
source("starterFilesDiscreate/code_int_protein_Na.R")
source("starterFilesDiscreate/code_int_protein_phase.R")

source("starterFilesDiscreate/code_int_mrna_protein_carbon.R")
source("starterFilesDiscreate/code_int_mrna_protein_Mg.R")
source("starterFilesDiscreate/code_int_mrna_protein_Na.R")
source("starterFilesDiscreate/code_int_mrna_protein_phase.R")


# call mrna expo
source("starterFilesDiscreate/code_mrna_exp_carbon.R")
source("starterFilesDiscreate/code_mrna_exp_Mg.R")
source("starterFilesDiscreate/code_mrna_exp_Na.R")

# call mrna sta
source("starterFilesDiscreate/code_mrna_sta_carbon.R")
source("starterFilesDiscreate/code_mrna_sta_Mg.R")
source("starterFilesDiscreate/code_mrna_sta_Na.R")

# call protein exp
source("starterFilesDiscreate/code_protein_exp_carbon.R")
source("starterFilesDiscreate/code_protein_exp_Mg.R")
source("starterFilesDiscreate/code_protein_exp_Na.R")

# call protein sta
source("starterFilesDiscreate/code_protein_sta_carbon.R")
source("starterFilesDiscreate/code_protein_sta_Mg.R")
source("starterFilesDiscreate/code_protein_sta_Na.R")
