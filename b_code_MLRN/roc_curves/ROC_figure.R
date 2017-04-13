# ROC figure generation

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

# Machine learning
require("ROCR")

# Batch Correction
require("sva") # only for machine learning
###*****************************


###*****************************
# load data
load(file = "roc_curves/mRNA.Rdata")
assign(x = "ROC_DF_mRNA", value = ROC_DF)

load(file = "roc_curves/protein.Rdata")
assign(x = "ROC_DF_protein", value = ROC_DF)

remove(ROC_DF)
###*****************************


###*****************************
# Calculating Average by probabilities
ROC_DF_mRNA$alpha.values[1]
ROC_DF_mRNA$x.values[1]
ROC_DF_mRNA$y.values[1]
###*****************************

prob_list_mRNA<-(sort(unlist(as.vector(ROC_DF_mRNA$alpha.values)),decreasing = T))
prob_list_mRNA=unique(prob_list_mRNA[prob_list_mRNA<1.1])
prob_list_mRNA_downsample=prob_list_mRNA[seq(from=1, to=length(prob_list_mRNA), by=100)]




meanXY2<-function(data)
{
  minLength=32
  
  for(counter05 in 1:length(data$x.values))
  {
    xx_list_=data$x.values[[counter05]]
    if(length(xx_list_)<minLength)
    {xx_list_=c(xx_list_, rep(1, minLength-length(xx_list_)))}
    data$x.values2[[counter05]]=xx_list_
    
    yy_list_=data$y.values[[counter05]]
    if(length(yy_list_)<minLength)
    {yy_list_=c(yy_list_, rep(1, minLength-length(yy_list_)))}
    data$y.values2[[counter05]]=yy_list_
  }
  
  xx_list=rowMeans(as.matrix(data.frame(data$x.values2)))
  yy_list=rowMeans(as.matrix(data.frame(data$y.values2)))
  
  if(length(xx_list)<minLength)
  {xx_list=c(xx_list, rep(1, minLength-length(xx_list)))}
  
  if(length(yy_list)<minLength)
  {yy_list=c(yy_list, rep(1, minLength-length(yy_list)))}
  
  XY=data.frame(x=xx_list,y=yy_list)
  return(XY)
}


###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="linear")->ROC_linear_mRNA

result_linear_mRNA=meanXY2(ROC_linear_mRNA)
result_linear_mRNA%>%
  dplyr::mutate(model="linear", dataType="mRNA")->result_linear_mRNA
###*****************************


###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="radial")->ROC_radial_mRNA

result_radial_mRNA=meanXY2(ROC_radial_mRNA)
result_radial_mRNA%>%
  dplyr::mutate(model="radial", dataType="mRNA")->result_radial_mRNA
###*****************************

###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="sigmoidal")->ROC_sigmoidal_mRNA

result_sigmoidal_mRNA=meanXY2(ROC_sigmoidal_mRNA)
result_sigmoidal_mRNA%>%
  dplyr::mutate(model="sigmoidal",dataType="mRNA")->result_sigmoidal_mRNA
###*****************************

###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="RF")->ROC_RF_mRNA

result_RF_mRNA=meanXY2(ROC_RF_mRNA)
result_RF_mRNA%>%
  dplyr::mutate(model="RF", dataType="mRNA")->result_RF_mRNA
###*****************************


###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="linear")->ROC_linear_protein

result_linear_protein=meanXY2(ROC_linear_protein)
result_linear_protein%>%
  dplyr::mutate(model="linear", dataType="protein")->result_linear_protein
###*****************************


###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="radial")->ROC_radial_protein

result_radial_protein=meanXY2(ROC_radial_protein)
result_radial_protein%>%
  dplyr::mutate(model="radial", dataType="protein")->result_radial_protein
###*****************************

###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="sigmoidal")->ROC_sigmoidal_protein

result_sigmoidal_protein=meanXY2(ROC_sigmoidal_protein)
result_sigmoidal_protein%>%
  dplyr::mutate(model="sigmoidal", dataType="protein")->result_sigmoidal_protein
###*****************************

###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="RF")->ROC_RF_protein

result_RF_protein=meanXY2(ROC_RF_protein)
result_RF_protein%>%
  dplyr::mutate(model="RF", dataType="protein")->result_RF_protein
###*****************************




all<-dplyr::bind_rows(result_linear_mRNA,result_radial_mRNA,result_sigmoidal_mRNA, result_RF_mRNA,
                      result_linear_protein,result_radial_protein,result_sigmoidal_protein, result_RF_protein)
ggplot(all,aes(x=x,y=y,group=model, colour=model))+facet_grid(.~dataType)+geom_line()
