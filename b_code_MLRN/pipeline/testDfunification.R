performanceDfSVM %>%
  dplyr::mutate(gamma=ifelse(kernel=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error)%>%
  dplyr::group_by(kernel, runNum, gamma, cost) %>%
  dplyr::summarize(error=unique(error), dispersion=unique(dispersion), 
                   performance=unique(performance))%>%
  dplyr::group_by(kernel, runNum) %>%
  dplyr::select(error, performance) %>%
  dplyr::filter(performance==max(performance))%>%
  dplyr::summarize(performance=unique(performance), error=unique(error))->q

performanceDf %>%
  dplyr::select(-kernel)%>%
  dplyr::mutate(gamma=ifelse(model=="linear",NA,gamma))%>%
  dplyr::mutate(performance=1-error)%>%
  dplyr::group_by(model, runNum, gamma, cost, nodesize, mtry, ntree) %>%
  dplyr::summarize(error=unique(error), dispersion=unique(dispersion), 
                   performance=unique(performance))%>%
  dplyr::group_by(model, runNum) %>%
  dplyr::select(error, performance) %>%
  dplyr::filter(performance==max(performance))%>%
  dplyr::summarize(performance=unique(performance), error=unique(error))->q2

ggplot(q2, aes(x=model, y=performance, group=model))+
  geom_violin(fill="grey80")

