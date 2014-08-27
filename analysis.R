rm(list=ls())
library(TSA)
library(mgcv)
library(plyr)
source('R/functions-analyses.R')
source('R/structure.R')
source('database.R')

##################
## running GAMM ##
##################
tab      <-  expand.grid(resp=names(response), pred=names(predictvar), stringsAsFactors=FALSE)
results  <-  vector(mode='list', length=nrow(tab))
names(results)  <-  paste0(tab$resp, '_', tab$pred)

for(i in 1:nrow(tab)) {
	resp  <-  response[ , tab$resp[i]]
	pred  <-  predictvar[ , tab$pred[i]]
	results[[i]] <- gamm(resp ~ s(pred), family=poisson(lonk=log), random=list(province=~1)) 
}
rm(resp, pred)

#######################
## summarize results ##
#######################
sum_tab  <-  ldply(results, summaryGAMM)
write.csv(sum_tab, 'output/data/results_summary_genera.csv', row.names=FALSE)

save.image('output/data/results.RData')