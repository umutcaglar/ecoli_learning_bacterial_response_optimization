# Random loop chose file

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
set.seed(14159)
###*****************************

listAxis=0
listScore=0
lastScore=0
counter02a=1


while(counter02a==1|listScore[counter02a]>lastScore)
{
  loopScore=rep(x = 0,20)
  for(counter02b in 1:20){loopScore[counter02b]= runif(1)*counter02a}
  listAxis[counter02a+1]=which(loopScore==max(loopScore))
  listScore[counter02a+1]=max(loopScore)
  lastScore=listScore[counter02a]
  counter02a=counter02a+1
  browser()
}