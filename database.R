data  <-  read.csv("data/data.csv", header=TRUE, stringsAsFactors=FALSE)


#######################
## response variable ##
#######################
data$sumcover[data$sumcover > 100]        <-  100
data$Competitive[data$Competitive > 100]  <-  100

response0  <-  data[,c("sumcover", "Competitive", "StressTolerant", "Weedy", "Generalist")] * 0.01
response1  <-  data[, c("FEve", "FDis", "FDiv", "RaoQ")]
#response   <-  cbind(response0, response1)
#rm(response0, response1)

response  <-  data.frame(data[,14])
colnames(response)<-"no.genera"



#########################
## predictor variables ##
#########################
predictvar  <-  data[,c("bal","exp","oc","rnr","finmodel","sst_kurtosis","meanssta","meantsa","PAR_50P","PAR_98p","skewnes","SST_2p","SST_50p","SST_98p","SST_p50","sstafreq","sstamax","stdev","tsafreq","tsamax","NCEAS_Model","nonseasonalvar","LHSA")] ## continuous predictor variables

###################
## random effect ##
###################
province  <-  data$Province
