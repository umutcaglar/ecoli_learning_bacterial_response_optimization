
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