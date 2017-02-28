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
trace_mrna=c(79.78, 76.78, 70.80, 64.80, 84.40, 29.90, 97.60,
             74.17, 52.70, 56.70, 66.45, 29.80, 2.10, 34.20, 76.80, 64.30)
trace_protein=c(38.50,76.22,24.10,68.20,73.43,65.60,100.00, 
                7.50, 46.60, 35.40, 39.80, 50.70, 66.70, 100.00, 64.20, 21.10 )
trace_int_mrna=c(0.30, 73.75, 61.20, 55.00, 80.30, 34.20, 91.60,
                 3.50, 65.85, 60.20, 28.80, 43.80, 5.20, 38.10, 72.20, 28.90)
trace_int_protein=c(37.30, 74.22,  9.10, 70.80, 79.03, 65.70, 99.90,
                    4.10, 48.85, 27.70, 40.90, 48.60, 67.50, 98.80, 63.60, 4.50)
trace_int_mrna_protein=c(1.60, 77.08, 23.30, 72.30, 74.03, 64.00, 99.60,
                         10.20, 63.65, 66.90, 28.30, 49.20, 54.30, 82.20, 63.80, 38.40)

complex_mrna=data.frame(Percentage = trace_mrna, condition="complex_mrna", dataType= "mrna", dataSet = "All data")
complex_protein=data.frame(Percentage = trace_protein, condition="complex_protein", dataType="protein", dataSet = "All data")
complex_int_mrna=data.frame(Percentage = trace_int_mrna, condition="complex_int_mrna", dataType="mrna", dataSet = "Intersection")
complex_int_protein=data.frame(Percentage = trace_int_protein, condition="complex_int_protein", dataType="protein", dataSet = "Intersection")
complex_int_mrna_protein=data.frame(Percentage = trace_int_mrna_protein, condition="complex_int_mrna_protein", dataType= "mrna & protein", dataSet = "Intersection")
###*****************************


###*****************************
# R bind all DFs
dplyr::bind_rows(complex_mrna,
                 complex_protein,
                 complex_int_mrna,
                 complex_int_protein,
                 complex_int_mrna_protein)->combined
###*****************************

###*****************************
# Generating DF
combined %>%
  dplyr::group_by(condition, dataType, dataSet) %>%
  dplyr::summarise(sumPercentage=sum(Percentage), 
                   success_v1=sum(Percentage)/100,
                   totLength=length(Percentage)) %>%
  dplyr::mutate(success_v2=sumPercentage/(100*totLength)) %>%
  dplyr::mutate(success_v3=1+(sumPercentage-100)/(100* (totLength-1) )) ->combined_summary
###*****************************


###*****************************
# Generate figure
fig01<-ggplot(data = combined_summary, mapping = aes(x=dataSet,y=success_v3, color=dataType, group=dataType))+
  geom_point(size=2, alpha=.8)+
  geom_line()+
  geom_hline(yintercept = 1, colour="grey40", linetype = 2)+
  geom_hline(yintercept = 2, colour="grey40", linetype = 2)+
  xlab("Complex Tests") + ylab("Success") +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0.9,2.1), 
                     breaks = c(1,2), 
                     labels = c("Random","Perfect"))

print(fig01)







###*****************************
# save figure 3 (WINNER)
cowplot::save_plot(filename = "fig05_overalTrends.pdf",plot = fig03,ncol = 2, nrow = 1)
###*****************************