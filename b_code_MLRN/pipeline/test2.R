###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
set.seed(14159)
###*****************************

a=sample(1:5,100,replace=TRUE);
b=sample(1:5,100,replace=TRUE); 

inputs=sort(unique(a))

for (counter04 in 1:length(inputs))
{
  testFor=inputs[counter04]
  
  a=as.vector(a); b=as.vector(b)
  q=as.data.frame(a,b); 
  
  sum(a==testFor & b==testFor)->TP2
  sum(a!=testFor & b==testFor)->FP2
  sum(a==testFor & b!=testFor)->FN2
  
  
  q1<-q
  
  q1 %>%
    dplyr::mutate(selected_a=ifelse(a==testFor,1,0))%>%
    dplyr::mutate(selected_b=ifelse(b==testFor,1,0))%>%
    dplyr::mutate(TP=ifelse(selected_b==1&selected_a==1,1,0))%>%
    dplyr::mutate(FP=ifelse(selected_b==1&selected_a==0,1,0))%>%
    dplyr::mutate(FN=ifelse(selected_b==0&selected_a==1,1,0))%>%
    dplyr::mutate(TN=ifelse(selected_b==0&selected_a==0,1,0))->q1
  
  sum(q1$TP)->TP
  sum(q1$FP)->FP
  sum(q1$FN)->FN
  
  if(counter04==1)
  {F1=(2*TP)/(2*TP+FP+FN)}
  if(counter04!=1)
  {F1[counter04]=(2*TP)/(2*TP+FP+FN)}
  browser()
}

F1_err=-mean(F1)+1