
###*****************************
# Make the names look better
# Rename Updated columns
temp<-list("Exponential"="exponential",
           "Late-Stationary"="late_stationary", 
           "Stationary"="stationary")
levels(condition$growthPhase)<-temp

temp<-list("Glucose"="glucose",
           "Glycerol"="glycerol",
           "Gluconate"="gluconate",
           "Lactate"="lactate" )
levels(condition$carbonSource)<-temp

temp<-list("Low Mg"="lowMg",
           "Base Mg"="baseMg",
           "High Mg"="highMg")
levels(condition$Mg_mM_Levels)<-temp

temp<-list("Base Na"="baseNa",
           "High Na"="highNa")
levels(condition$Na_mM_Levels)<-temp
###*****************************


###*****************************
# Arrange and rename order of factors 

# Initial name list for different variables (in correct order)
growthPhaseVector=c("Exponential","Stationary","Late-Stationary")
carbonSourceVector=c("Glucose","Glycerol","Gluconate","Lactate")
Mg_mM_LevelsVector=c("Low Mg","Base Mg","High Mg")
Na_mM_LevelsVector=c("Base Na","High Na")

# Updated name list for different variables (in correct order)
growthPhaseVector_Short=c("Exp","Sta","Lt.Sta")
carbonSourceVector_Short=c("Glu","Gly","Glc","Lac")
Mg_mM_LevelsVector_Short=c("L.Mg","B.Mg","H.Mg")
Na_mM_LevelsVector_Short=c("B.Na","H.Na")

# Generated vectior lists associated with vectors and short vectors
vectorList=list(growthPhase=growthPhaseVector,
                carbonSource=carbonSourceVector,
                Mg_mM_Levels=Mg_mM_LevelsVector,
                Na_mM_Levels=Na_mM_LevelsVector)

vectorList_Short=list(growthPhase_Short=growthPhaseVector_Short,
                      carbonSource_Short=carbonSourceVector_Short,
                      Mg_mM_Levels_Short=Mg_mM_LevelsVector_Short,
                      Na_mM_Levels_Short=Na_mM_LevelsVector_Short)

# Put column labels in order 
condition$growthPhase <- factor(condition$growthPhase, 
                                levels=growthPhaseVector)
condition$carbonSource <- factor(condition$carbonSource, 
                                 levels=carbonSourceVector)
condition$Mg_mM_Levels <- factor(condition$Mg_mM_Levels, 
                                 levels=Mg_mM_LevelsVector)
condition$Na_mM_Levels <- factor(condition$Na_mM_Levels, 
                                 levels=Na_mM_LevelsVector)

# Generate updated columns
condition %>%
  dplyr::mutate(growthPhase_Short=growthPhase,
                carbonSource_Short=carbonSource,
                Mg_mM_Levels_Short=Mg_mM_Levels,
                Na_mM_Levels_Short=Na_mM_Levels)->condition


# Rename Updated columns
temp<-as.list(vectorList$growthPhase)
names(temp)<-vectorList_Short$growthPhase
levels(condition$growthPhase_Short)<-temp

temp<-as.list(vectorList$carbonSource)
names(temp)<-vectorList_Short$carbonSource_Short
levels(condition$carbonSource_Short)<-temp

temp<-as.list(vectorList$Mg_mM_Levels)
names(temp)<-vectorList_Short$Mg_mM_Levels_Short
levels(condition$Mg_mM_Levels_Short)<-temp

temp<-as.list(vectorList$Na_mM_Levels)
names(temp)<-vectorList_Short$Na_mM_Levels_Short
levels(condition$Na_mM_Levels_Short)<-temp
###*****************************


###*****************************
# Generate "conditionInvestigated" and "conditionInvestigated_Short" data column in meta data
inputMetaDf=condition
if(length(testConditions)!=1)
{
  inputMetaDf$conditionInvestigated <- apply( inputMetaDf[,testConditions] , 1 , paste , collapse = "_") # generate the "conditionInvestigated" column
  expandedGrid<-expand.grid(vectorList[paste0(testConditions)]) # generate an expandedGrid DF 
  expandedGrid$conditionInvestigatedVector <- apply( expandedGrid[,paste0(testConditions)] , 1 , paste , collapse = "_") # generate a "conditionInvestigated" column in pandedGrid DF
  conditionInvestigatedVector=expandedGrid$conditionInvestigatedVector # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated" column)
  
  inputMetaDf$conditionInvestigated_Short <- apply( inputMetaDf[,paste0(testConditions,"_Short")] , 1 , paste , collapse = "_") # generate the "conditionInvestigated_Short" column
  expandedGrid_Short<-expand.grid(vectorList_Short[paste0(testConditions,"_Short")]) # generate an expandedGrid_Short DF 
  expandedGrid_Short$conditionInvestigatedVector_Short <- apply( expandedGrid_Short[,paste0(testConditions,"_Short")] , 1 , paste , collapse = "_") # generate a "conditionInvestigated_Short" column in pandedGrid_Short DF
  conditionInvestigatedVector_Short=expandedGrid_Short$conditionInvestigatedVector_Short # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated_Short" column)
  
  # Import the order of labels to "conditionInvestigated" column
  inputMetaDf$conditionInvestigated <- factor(inputMetaDf$conditionInvestigated, 
                                              levels=conditionInvestigatedVector)
  inputMetaDf$conditionInvestigated<-droplevels(inputMetaDf$conditionInvestigated)
  
  # Import the order of labels to "conditionInvestigated_Short" column
  inputMetaDf$conditionInvestigated_Short <- factor(inputMetaDf$conditionInvestigated_Short, 
                                                    levels=conditionInvestigatedVector_Short)
  inputMetaDf$conditionInvestigated_Short<-droplevels(inputMetaDf$conditionInvestigated_Short)
}


if(length(testConditions)==1)
{
  inputMetaDf%>% 
    dplyr::mutate_(conditionInvestigated=testConditions)->inputMetaDf
  
  if(paste0(testConditions,"_Short") %in% colnames(inputMetaDf))
  {
    inputMetaDf%>% 
      dplyr::mutate_(conditionInvestigated_Short=paste0(testConditions,"_Short"))->inputMetaDf
  }
  if(!paste0(testConditions,"_Short") %in% colnames(inputMetaDf))
  {
    inputMetaDf%>% 
      dplyr::mutate_(conditionInvestigated_Short=testConditions)->inputMetaDf
  }
  
  if(type_svmChoice == "C-classification")
  {
    expandedGrid=as.data.frame(levels(inputMetaDf$conditionInvestigated))
    colnames(expandedGrid)<-testConditions
    expandedGrid[[testConditions]]<-factor(expandedGrid[[testConditions]], 
                                           levels=as.vector(expandedGrid[[testConditions]]))
    expandedGrid %>%
      dplyr::mutate_(conditionInvestigatedVector=testConditions)->expandedGrid
    
    expandedGrid_Short=as.data.frame(levels(inputMetaDf$conditionInvestigated_Short))
    colnames(expandedGrid_Short)<-paste0(testConditions,"_Short")
    expandedGrid_Short[[paste0(testConditions,"_Short")]]<-factor(expandedGrid_Short[[paste0(testConditions,"_Short")]], 
                                                                  levels=as.vector(expandedGrid_Short[[paste0(testConditions,"_Short")]]))
    expandedGrid_Short %>%
      dplyr::mutate_(conditionInvestigatedVector_Short=paste0(testConditions,"_Short"))->expandedGrid_Short
    
    conditionInvestigatedVector=expandedGrid$conditionInvestigatedVector # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated" column)
    conditionInvestigatedVector_Short=expandedGrid_Short$conditionInvestigatedVector_Short # save this column as vector (Which is a correct oreder of labels for "conditionInvestigated_Short"
    
    # Import the order of labels to "conditionInvestigated" column
    inputMetaDf$conditionInvestigated <- factor(inputMetaDf$conditionInvestigated, 
                                                levels=conditionInvestigatedVector)
    inputMetaDf$conditionInvestigated<-droplevels(inputMetaDf$conditionInvestigated)
    
    # Import the order of labels to "conditionInvestigated_Short" column
    inputMetaDf$conditionInvestigated_Short <- factor(inputMetaDf$conditionInvestigated_Short, 
                                                      levels=conditionInvestigatedVector_Short)
    inputMetaDf$conditionInvestigated_Short<-droplevels(inputMetaDf$conditionInvestigated_Short) 
  }
}

factorOrder<-levels(inputMetaDf$conditionInvestigated)
factorOrder_Short<-levels(inputMetaDf$conditionInvestigated_Short)
###*****************************


###*****************************
# Add a combined vector of all conditions
inputMetaDf%>% 
  dplyr::mutate(growthTime_hrs = ifelse(growthTime_hr<8 & growthTime_hr>=5, "[5-8)", NA),
                growthTime_hrs = ifelse(growthTime_hr<=10 & growthTime_hr>=8, "[8-10]", growthTime_hrs),
                growthTime_hrs = ifelse(is.na(growthTime_hrs),growthTime_hr,growthTime_hrs))->inputMetaDf


inputMetaDf%>%
  tidyr::unite_(col= "allConditionsTogether_Short" , 
                from = similarDataClassifierForBatch, 
                sep="_", 
                remove=FALSE)->inputMetaDf
###*****************************


###*****************************
# Remove Unnecessary columns from meta data 
inputMetaDf %>%
  dplyr::select(-uniqueCondition, -uniqueCondition02, -doublingTimeMinutes,
                -doublingTimeMinutes.95m, -doublingTimeMinutes_95p, -rSquared, -cellTotal,
                -cellsPerTube)->inputMetaDf
###*****************************


###*****************************
# Hand Made Error Function
F1ScoreErr<-function(y,prediction)
{
  beta=1
  y=make.names(y)
  prediction=make.names(prediction)
  a=as.vector(y); b=as.vector(prediction)
  
  inputs=sort(unique(c(a,b)))
  
  TP=c();
  FP=c();
  FN=c();
  F1=c();
  
  for (counter04 in 1:length(inputs))
  {
    testFor=inputs[counter04]
    
    sum(a==testFor & b==testFor)->TP[counter04]
    sum(a!=testFor & b==testFor)->FP[counter04]
    sum(a==testFor & b!=testFor)->FN[counter04]
    
    F1[counter04]=(2*TP[counter04])/(2*TP[counter04]+FP[counter04]+FN[counter04])
    if(0==2*TP[counter04]+FP[counter04]+FN[counter04]){F1[counter04]==0}
  }
  
  # print(paste0("TP: ", paste0(TP, collapse = " ")));
  # print(paste0("FP: ", paste0(FP, collapse = " ")));
  # print(paste0("FN: ", paste0(FN, collapse = " ")));
  # print(paste0("F1: ", paste0(F1, collapse = " ")));
  
  # Controls
  sum_TP = sum(TP, na.rm =T)
  sum_FP = sum(FP, na.rm =T)
  sum_FN = sum(FN, na.rm =T)
  if(sum_FP!=sum_FN){browser()}
  if(length(y)!=length(prediction)){browser()}
  if(length(y)!=sum_TP + sum_FP){browser()}
  
  F1_err=1-mean(F1, na.rm =T)
  # print(paste0("F1_err: ", F1_err));
  
  # browser()
  return(F1_err)
}






F1ScoreErr1<-function(y,prediction)
{
  beta=1
  y=make.names(y)
  prediction=make.names(prediction)
  a=as.vector(y); b=as.vector(prediction)
  
  inputs=sort(unique(c(a,b)))
  #print(paste0("inputs: ", paste0(inputs, collapse = " ")))
  
  TP=c();
  FP=c();
  FN=c();
  for (counter04 in 1:length(inputs))
  {
    testFor=inputs[counter04]
    
    sum((a==testFor) & (b==testFor))->TP[counter04]
    sum((!a==testFor) & (b==testFor))->FP[counter04]
    sum((a==testFor) & (!b==testFor))->FN[counter04]
  }
  
  #print(paste0("TP: ", paste0(TP, collapse = " ")));
  #print(paste0("FP: ", paste0(FP, collapse = " ")));
  #print(paste0("FN: ", paste0(FN, collapse = " ")));
  
  # Controls
  sum_TP = sum(TP, na.rm =T)
  sum_FP = sum(FP, na.rm =T)
  sum_FN = sum(FN, na.rm =T)
  if(sum_FP!=sum_FN){browser()}
  if(length(y)!=length(prediction)){browser()}
  if(length(y)!=sum_TP + sum_FP){browser()}
  
  #print(paste0("sum FP: ", sum_FP))
  #print(paste0("sum FN: ", sum_FN))
  
  PRE_vec = TP/(TP+FP);
  REC_vec = TP/(TP+FN);
  
  #print(paste0("PRE_vec: ", paste0(PRE_vec, collapse = " ")))
  #print(paste0("REC_vec: ", paste0(REC_vec, collapse = " ")))
  
  PRE_m = mean(PRE_vec, na.rm=T);
  REC_m = mean(REC_vec, na.rm=T);
  
  #print(paste0("PRE_m: ",PRE_m))
  #print(paste0("REC_m: ",REC_m))
  
  Fscore_m = ((beta^2+1)*PRE_m*REC_m)/((beta^2)*PRE_m+REC_m);
  if(PRE_m+REC_m==0){Fscore_m = 0}
  #print(paste0("Fscore_m1: ",Fscore_m))
  
  F1_err=1-Fscore_m
  return(F1_err)
}

# F1ScoreErr2<-function(y,prediction)
# {
#   beta=1
#   y=make.names(y)
#   prediction=make.names(prediction)
#   a=as.vector(y); b=as.vector(prediction)
#   
#   q<-data.frame(a=a,b=b)
#   
#   q[[1]]<-factor(as.vector(q[[1]]))
#   q[[2]]<-factor(as.vector(q[[2]]))
#   
#   combinedLevels=sort(union(levels(q[[1]]),levels(q[[2]])))
#   combinedLevelsDF_a=data.frame(a=combinedLevels)
#   combinedLevelsDF_b=data.frame(b=combinedLevels)
#   
#   q[[1]] <- factor(q[[1]], levels=combinedLevels)
#   q[[2]] <- factor(q[[2]], levels=combinedLevels)
#   
#   q %>%
#     dplyr::group_by(b) %>% 
#     dplyr::filter(a!=b) %>% 
#     dplyr::summarize(FP=n())%>%
#     dplyr::left_join(combinedLevelsDF_b, ., by = "b")%>%
#     dplyr::mutate(FP=ifelse(is.na(FP),0,FP))%>%
#     dplyr::rename(variable=b)->FP
#   sum(FP$FP)
#   
#   q %>%
#     dplyr::group_by(a) %>% 
#     dplyr::filter(a!=b) %>% 
#     dplyr::summarize(FN=n()) %>%
#     dplyr::left_join(combinedLevelsDF_a, ., by = "a")%>%
#     dplyr::mutate(FN=ifelse(is.na(FN),0,FN))%>%
#     dplyr::rename(variable=a)->FN
#   sum(FN$FN)
#   
#   q %>%
#     dplyr::group_by(a) %>% 
#     dplyr::filter(a==b)%>% 
#     dplyr::summarize(TP=n())%>%
#     dplyr::left_join(combinedLevelsDF_a, ., by = "a")%>%
#     dplyr::mutate(TP=ifelse(is.na(TP),0,TP))%>%
#     dplyr::rename(variable=a)->TP
#   
#   dplyr::left_join(TP,FP, by = "variable") %>%
#     dplyr::left_join(.,FN, by = "variable")->summaryDF
#   
#   summaryDF %>%
#     dplyr::mutate(precision=TP/(TP+FP),
#                   recall=TP/(TP+FN))->summaryDF
#   
#   PRE_m = mean(summaryDF$precision, na.rm=T);
#   REC_m = mean(summaryDF$recall, na.rm=T);
#   
#   Fscore_m = ((beta^2+1)*PRE_m*REC_m)/((beta^2)*PRE_m+REC_m);
#   if(PRE_m+REC_m==0){Fscore_m = 0}
#   
#   #print(paste0("Fscore_m2: ", Fscore_m))
#   F1_err=1-Fscore_m
#   return(F1_err)
# }

#sourceCpp("pipeline/f1ScoreFunction.cpp")
#sourceCpp("pipeline/f1ScoreFunctionCorrected.cpp")
###*****************************