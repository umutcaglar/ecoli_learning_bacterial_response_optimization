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
prob_list_mRNA=unique(prob_list_mRNA[prob_list_mRNA<20])
prob_list_mRNA_downsample=prob_list_mRNA[seq(from=1, to=length(prob_list_mRNA), by=100)]

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


meanXY2<-function(data)
{
  stepSize=-0.01
  probList_ex=list()
  
  for(counter05 in 1:nrow(data))
  {
    old_scale=data$alpha.values[[counter05]]
    old_scale_=c(data$alpha.values[[counter05]],0)
    
    newScale=c(Inf,seq(from=1, to=0, by=stepSize))
    old_scale_ex=rep(NaN,length(newScale))
    for(counter04 in 2:length(old_scale_))
    {
      old_scale_ex[(newScale>=old_scale_[counter04] & 
                      old_scale_[counter04-1]>newScale)]=old_scale_[counter04-1]
    }
    
    
    old_scale_ex[1]=Inf; 
    if(any(is.na(old_scale_ex))){browser()}
    
    probDf_ex=data.frame(newScale=newScale, oldScale=old_scale_ex)
    
    probDf=data.frame(x.val=data$x.values[[counter05]],
               y.val=data$y.values[[counter05]],
               oldScale=old_scale)
    
    probDf_ex<-dplyr::left_join(probDf_ex, probDf, by = "oldScale")
    probDf_ex %>%
      dplyr::mutate(dataNum=counter05,
                    distinctRuns=data$distinctRuns[[counter05]],
                    model=data$model[[counter05]],
                    chosenCondition=data$chosenCondition[[counter05]])->probDf_ex
    probList_ex[[counter05]]=probDf_ex
  }
  
  prob_ex<-dplyr::bind_rows(probList_ex)
  
  return(prob_ex)
}


###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="linear")->ROC_linear_mRNA

result_linear_mRNA=meanXY2(ROC_linear_mRNA)
result_linear_mRNA %>%
  dplyr::mutate(dataType="mRNA")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_linear_mRNA_sum
###*****************************


###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="radial")->ROC_radial_mRNA

result_radial_mRNA=meanXY2(ROC_radial_mRNA)
result_radial_mRNA %>%
  dplyr::mutate(dataType="mRNA")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_radial_mRNA_sum
###*****************************

###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="sigmoidal")->ROC_sigmoidal_mRNA

result_sigmoidal_mRNA=meanXY2(ROC_sigmoidal_mRNA)
result_sigmoidal_mRNA %>%
  dplyr::mutate(dataType="mRNA")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_sigmoidal_mRNA_sum
###*****************************

###*****************************
ROC_DF_mRNA%>%
  dplyr::filter(model=="RF")->ROC_RF_mRNA

result_RF_mRNA=meanXY2(ROC_RF_mRNA)
result_RF_mRNA %>%
  dplyr::mutate(dataType="mRNA")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_RF_mRNA_sum
###*****************************


###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="linear")->ROC_linear_protein

result_linear_protein=meanXY2(ROC_linear_protein)
result_linear_protein %>%
  dplyr::mutate(dataType="protein")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_linear_protein_sum
###*****************************


###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="radial")->ROC_radial_protein

result_radial_protein=meanXY2(ROC_radial_protein)
result_radial_protein %>%
  dplyr::mutate(dataType="protein")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_radial_protein_sum
###*****************************

###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="sigmoidal")->ROC_sigmoidal_protein

result_sigmoidal_protein=meanXY2(ROC_sigmoidal_protein)
result_sigmoidal_protein %>%
  dplyr::mutate(dataType="protein")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_sigmoidal_protein_sum
###*****************************

###*****************************
ROC_DF_protein%>%
  dplyr::filter(model=="RF")->ROC_RF_protein

result_RF_protein=meanXY2(ROC_RF_protein)
result_RF_protein %>%
  dplyr::mutate(dataType="protein")%>%
  dplyr::group_by(newScale)%>%
  dplyr::summarise(x.val=mean(x.val),y.val=mean(y.val),model=unique(model), dataType=unique(dataType))->result_RF_protein_sum
###*****************************




all<-dplyr::bind_rows(result_linear_mRNA_sum,result_radial_mRNA_sum,result_sigmoidal_mRNA_sum, result_RF_mRNA_sum,
                      result_linear_protein_sum,result_radial_protein_sum,result_sigmoidal_protein_sum, result_RF_protein_sum)

all$model <- factor(all$model, levels = c("radial", "sigmoidal", "linear", "RF"))

fig01<-ggplot(all,aes(x=x.val, y=y.val, group=model, colour=model))+
  facet_grid(.~dataType)+
  geom_line(size=1)+
  xlab("tpr")+ylab("fpr")

print(fig01)

cowplot::save_plot(filename = "../b_figures/ROC_curves.jpeg", plot = fig01, ncol = 2)



###*****************************


result_linear_mRNA %>%
  dplyr::mutate(dataType="mRNA")%>%
  dplyr::group_by(newScale,chosenCondition)%>%
  dplyr::summarise(x.val=mean(x.val),
                   y.val=mean(y.val),
                   model=unique(model), 
                   dataType=unique(dataType))->q

fig02<-ggplot(q,aes(x=x.val, y=y.val, group=chosenCondition, colour=chosenCondition))+
  geom_line(size=1)+
  xlab("tpr")+ylab("fpr")

print(fig02)
###*****************************