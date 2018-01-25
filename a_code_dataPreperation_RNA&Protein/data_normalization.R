## DeSeq Normalization Cleaned 03

# The aim of the code is to generate normalized data matrix.
# The work flow compses of four parts
# Pick up the samples
# pick up the rows
# calculate size factors
# do the normalization


###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response_optimization/a_code_dataPreperation_RNA&Protein/"))} # mac computer
###*****************************


###*****************************
# DOWNLOAD LIBRARIES
require("Biobase") 
require("DESeq2")
require("dplyr")
require("tidyr")
###*****************************


###*****************************
#Load Functions
source("../a_code_dataPreperation_RNA&Protein/data_filter_normalization_functions.R")
###*****************************


###*****************************
saveFiles=TRUE
runDeSeqForDifExp=FALSE
# The data filtering function that controls sub functions.
mainData=filter_data(dataType = "mrna", 
                     # can be "rna", "mrna", "protein", "protein_wo_NA", 
                     # "int_mrna_protein", "int_mrna", "int_protein"
                     badDataSet = "set00", # can be "set00",set01","set02", "set03"
                     # referenceParameters can be a vector like
                     # c("growthPhase", "Mg_mM_Levels", "Na_mM_Levels", "carbonSource", "experiment")
                     referenceParameters=c("growthPhase",
                                           "Mg_mM_Levels", 
                                           "Na_mM_Levels", 
                                           "carbonSource", 
                                           "experiment"),
                     # referenceLevels can be a vector like
                     # c("exponential", "baseMg", "baseNa", "glucose", "glucose_time_course")
                     referenceLevels=c("exponential",
                                       "baseMg", 
                                       "baseNa", 
                                       "glucose", 
                                       "glucose_time_course"),
                     experimentVector = c("allEx"), # can be "Stc","Ytc","Nas","Agr","Ngr","Mgl","Mgh" // "allEx"
                     carbonSourceVector = "SYAN", # can be any sub combination of "SYAN"
                     MgLevelVector = c("allMg"), # can be "lowMg","baseMg","highMg" // "allMg"
                     NaLevelVector = c("allNa"), # can be "baseNa","highNa" // "allNa"
                     # can be "exponential","stationary","late_stationary" // "allPhase"
                     growthPhaseVector = c("allPhase"), 
                     filterGenes = c("noFilter"), # can be either "noFilter", or any combination of c("meanFilter", "maxFilter", "sdFilter", "noMatchFilter")
                     threshold=NA, # the threshold value for "meanFilter", "maxFilter", "sdFilter" can be  c(meanFilter=5,maxFilter=3,sdFilter=7)
                     roundData=TRUE,
                     sumTechnicalReplicates=TRUE,
                     deSeqSfChoice="p1Sf", # can be "regSf", "p1Sf", "noSf"
                     normalizationMethodChoice = "noNorm") # can be "vst", "rlog", "log10", "noNorm"
###*****************************


if(length(mainData)==2) # i.e the dataType!="comb_mrna_protein"
{
  ###*****************************
  #Decompose the container
  deseq_DataObj=mainData[[1]]
  objectName=mainData[[2]]
  ###*****************************
  
  
  ###*****************************
  res_df<-as.data.frame(assay(deseq_DataObj))
  metaData<-as.data.frame(colData(deseq_DataObj))
  ###*****************************
}

if(length(mainData)==3) # i.e the dataType=="comb_mrna_protein"
{
  objectName=mainData$objectName
  res_df=mainData$res_df
  metaData=mainData$metaData
}


###*****************************
# SAVE FILES
if(saveFiles){
  
  # save resDF
  objectName$initial="resDf"
  fileName=paste(objectName,collapse = "_")
  write.csv(x = res_df, 
            file = paste0("../a_results/",fileName,".csv"),
            row.names = TRUE,
            quote = FALSE)
  
  # save metaData
  objectName$initial="metaData"
  fileName=paste(objectName,collapse = "_")
  write.csv(x = metaData, 
            file = paste0("../a_results/",fileName,".csv"),
            row.names = FALSE,
            quote = FALSE)
}
###*****************************