# overalTrends R

# The aim of the file is to built a graph that shows the trends
# both for mrna and proteins in exponential and stationary phases

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
set.seed(14159)
###*****************************


###*****************************
# Set Working Directory
# One needs to arrange the correct pathway if this is not umut's computer ;)
if(as.vector(Sys.info()["effective_user"]=="umut"))
{setwd(paste0("/Users/umut/GitHub/ecoli_learning_bacterial_response/text/figures/figrue_preperation/"))} # mac computersldk
###*****************************


###*****************************
# install libraries
require("dplyr")
require("tidyr")

# Graphing
require("ggplot2")
require("cowplot")
###*****************************


###*****************************
#Load Functions
source("../../../a_code_dataPreperation_RNA&Protein/replace_fun.R")	
###*****************************


###*****************************
# Inporting Results
carbonSource_mrna_exp=data.frame(Percentage=c(99,48,33,83),condition="carbonSource_mrna_exp")
carbonSource_mrna_sta=data.frame(Percentage=c(92,23,28,1),condition="carbonSource_mrna_sta")
carbonSource_protein_exp=data.frame(Percentage=c(86,92,40,98),condition="carbonSource_protein_exp")
carbonSource_protein_sta=data.frame(Percentage=c(97,55,47,90),condition="carbonSource_protein_sta")

MgLevels_mrna_exp=data.frame(Percentage=c(70,89,53),condition="MgLevels_mrna_exp")
MgLevels_mrna_sta=data.frame(Percentage=c(46,83,28),condition="MgLevels_mrna_sta")
MgLevels_protein_exp=data.frame(Percentage=c(58,94,61),condition="MgLevels_protein_exp")
MgLevels_protein_sta=data.frame(Percentage=c(9,90,31),condition="MgLevels_protein_sta")

NaLevels_mrna_exp=data.frame(Percentage=c(98,40),condition="NaLevels_mrna_exp")
NaLevels_mrna_sta=data.frame(Percentage=c(98,47),condition="NaLevels_mrna_sta")
NaLevels_protein_exp=data.frame(Percentage=c(100,12),condition="NaLevels_protein_exp")
NaLevels_protein_sta=data.frame(Percentage=c(100,0),condition="NaLevels_protein_sta")
###*****************************


###*****************************
# R bind all DFs
dplyr::bind_rows(carbonSource_mrna_exp, carbonSource_mrna_sta, 
                 carbonSource_protein_exp, carbonSource_protein_sta,
                 MgLevels_mrna_exp, MgLevels_mrna_sta, 
                 MgLevels_protein_exp, MgLevels_protein_sta,
                 NaLevels_mrna_exp, NaLevels_mrna_sta, 
                 NaLevels_protein_exp, NaLevels_protein_sta)->combined
###*****************************

###*****************************
# Generating DF
combined %>%
  tidyr::separate(col = condition, into = c("variable","dataType","growthPhase"), sep = "_")%>%
  dplyr::mutate(variable = replace_fun(input_vector = variable, 
                                       initialVal = c("MgLevels","NaLevels","carbonSource"), 
                                       finalVal = c("Mg Levels","Na Levels","Carbon Source")))%>%
  dplyr::group_by(variable,dataType,growthPhase) %>%
  dplyr::mutate(success_v1=sum(Percentage)/100) %>%
  dplyr::mutate(success_v2=sum(Percentage)/(100*length(Percentage))) %>%
  dplyr::mutate(success_v3=1+(sum(Percentage)-100)/(100* (length(Percentage)-1) )) ->combined_summary

replace_fun(input_vector = combined_summary$variable, 
            initialVal = c("MgLevels","NaLevels","carbonSource"), 
            finalVal = c("Mg Levels","Na Levels","Carbon Source"))
###*****************************


###*****************************
# Generate figure
fig01<-ggplot(data = combined_summary, mapping = aes(x=growthPhase,y=success_v1, 
                                                     group=variable, colour=variable))+
  facet_grid(.~dataType)+
  geom_point(size=2, alpha=.8)+
  geom_line()+
  geom_hline(yintercept = 1, colour="grey40", linetype = 2)+
  xlab("Growth Phase") + ylab("Success") +
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5))+
  expand_limits(y = 0)+
  scale_color_manual(values = c("purple", "blue", "orange"))

print(fig01)


fig02<-ggplot(data = combined_summary, mapping = aes(x=growthPhase,y=success_v2, 
                                                     group=variable, colour=variable))+
  facet_grid(.~dataType)+
  geom_point(size=2, alpha=.8)+
  geom_line()+
  xlab("Growth Phase") + ylab("Success") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  expand_limits(y = 0)+
  scale_color_manual(values = c("purple", "blue", "orange"))


print(fig02)

fig03<-ggplot(data = combined_summary, mapping = aes(x=growthPhase,y=success_v3, 
                                                     group=variable, colour=variable))+
  facet_grid(.~dataType)+
  geom_point(size=2, alpha=.8)+
  geom_line()+
  geom_hline(yintercept = 1, colour="grey40", linetype = 2)+
  geom_hline(yintercept = 2, colour="grey40", linetype = 2)+
  xlab("Growth Phase") + ylab("Success") +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0.9,2.1), 
                     breaks = c(1,2), 
                     labels = c("Random","Perfect"))+
  scale_color_manual(values = c("purple", "blue", "orange"))

print(fig03)
###*****************************


###*****************************
# save figure 3 (WINNER)
cowplot::save_plot(filename = "fig05_overalTrends.pdf",plot = fig03,ncol = 2, nrow = 1)
###*****************************