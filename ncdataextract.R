library(rgeos)
library(raster)
library(rgdal)
library(dismo)
library(SDMTools)


######this code will allow for quick stacking of distinct files which are already multiband files themselves. Currently the package 'raster' can not stack files which are multibands. for example, each of the nc files with is a multiband raster, bands correspond to the number of monthly time series files. This function essentially is a modest generalization and alteration of the .quickStack function found in the raster package, tweaked to quick stack multiband files. Let’s call it qss.

qss <- function(files,mb=T,...)
{
if(mb){
ln <- nl <- bands.vec <- c()
for(p in 1:length(files)){
b <- brick(files[p],...)
ln <- c(ln, names(b))
nl <- c(nl, nlayers(b))
bands.vec <- c(bands.vec, 1:nlayers(b))
}
r <- raster(b,1)
r@data@haveminmax = FALSE
s <- stack(r)
files.vec <- rep(files,times=nl)
nbands.vec <- rep(nl,times=nl)
s@layers <- sapply(1:sum(nl), function(i) {
r@file@name <- files.vec[i]
r@file@nbands <- nbands.vec[i]
r@data@band <- bands.vec[i]
names(r) <- ln[i]
r
})
names(s) <- ln
} else {
r <- raster(files[1])
r@data@haveminmax = FALSE
s <- stack(r)
ln <- extension(basename(files), "")
nbands.vec <- bands.vec <- 1:length(files)
s@layers <- sapply(1:length(files), function(i) {
r@file@name <- files[i]
names(r) <- ln[i]
r
})
}
s
}

#########################################################
Extract environ variables from a nc files in a local drive
#########################################################
#Extract stress model values
cdata<-read.csv("~/Documents/UQ_Documents/Projects/GlobalCorals/CoralData/GLOBAL CORALS metrics for Maina_13June2014.csv")

exposure<-list.files(pattern='tif')
exposure
bal<-raster(exposure[1])
exp<-raster(exposure[4])
ocolor<-raster(exposure[10])
rnr<-raster(exposure[13])
model<-raster(exposure[7])


bal.extr<-extract(bal, cdata[,11:10])
exp.extr<-extract(exp, cdata[,11:10])
ocolor.extr<-extract(ocolor, cdata[,11:10])
rnr.extr<-extract(rnr, cdata[,11:10])
model.extr<-extract(model, cdata[,11:10])

##run extract with bufer for NA removal
bal.extr<-extract(bal, cdata[,11:10], buffer=10000, na.rm=TRUE, fun=median)
exp.extr<-extract(exp, cdata[,11:10], buffer=10000, na.rm=TRUE, fun=median)
ocolor.extr<-extract(ocolor, cdata[,11:10], buffer=10000, na.rm=TRUE, fun=median)
rnr.extr<-extract(rnr, cdata[,11:10], buffer=10000, na.rm=TRUE, fun=median)
model.extr<-extract(model, cdata[,11:10], buffer=10000, na.rm=TRUE, fun=median)


cdata$bal1<-bal.extr1
cdata$exp1<-exp.extr1
cdata$oc1<-ocolor.extr1
cdata$rnr1<-rnr.extr1
cdata$finmodel1<-model.extr1

###extract NCEAS models
setwd("Users/josephmaina/Documents/UQ_Documents/Projects/GlobalCorals/PredictorVars/NCEAS")

inorganic_lzw<-raster("inorganic_lzw.tif")
inorganic_lzw1<-raster("inorganic_lzw(1).tif")
model_class<-raster("model_class_wgs84_lzw.tif")
model<-raster("model_lzw.tif")
nutrient_lzw<-raster("nutrient_lzw.tif")
nutrient_lzw1<-raster("nutrient_lzw(1).tif"  )
ocean_acidification<-raster("ocean_acidification_lzw.tif")
organic_lzw<-raster("organic_lzw.tif" )
organic_lzw1<-raster("organic_lzw(1).tif" )
pollution<-raster("pollution_lzw.tif"   )
pollution1<-raster("pollution_lzw(1).tif"  )
population<-raster("population_lzw.tif" )
population1<-raster("population_lzw(1).tif"  )

inorganic_lzw<-extract(inorganic_lzw, cdata[,11:10])
inorganic_lzw1<-extract(inorganic_lzw1, cdata[,11:10])
model_class<-extract(model_class, cdata[,11:10])
model<-extract(model, cdata[11:10])
nutrient_lzw<-extract(nutrient_lzw, cdata[,11:10])
nutrient_lzw1<-extract(nutrient_lzw1, cdata[,11:10])
ocean_acidification<-extract(ocean_acidification, cdata[,11:10])
organic_lzw<-extract(organic_lzw, cdata[,11:10])
organic_lzw1<-extract(organic_lzw1, cdata[,11:10])
pollution<-extract(pollution, cdata[,11:10])
pollution1<-extract(pollution1, cdata[,11:10])
population<-extract(population, cdata[,11:10])
population1<-extract(population1, cdata[,11:10])

cdata$inorganic_lzw<-inorganic_lzw
cdata$inorganic_lzw1<-inorganic_lzw1
cdata$model_class<-model_class
cdata$model<-model
cdata$nutrient_lzw<-nutrient_lzw
cdata$nutrient_lzw1<-nutrient_lzw1
cdata$ocean_acidification<-ocean_acidification
cdata$organic_lzw<-organic_lzw
cdata$organic_lzw1<-organic_lzw1
cdata$pollution<-pollution
cdata$pollution1<-pollution1
cdata$population<-population
cdata$population1<-population1

ext<- extent(-18040095.196132, 18041068.724148, -9020981.826947, 9020067.372632)

model<- setExtent(model, ext, keepres=FALSE)

writeRaster(model, filename="model_nw.tif", format="GTiff", overwrite=TRUE)




##################################
##extract environmental variables
#################################
data<-read.csv("~/Documents/UQ_Documents/Projects/GlobalCorals/CoralData/GLOBAL CORALS metrics for Maina_13June2014.csv")


env.var<-list.files(pattern='tif')
env.var.stack1<-stack(env.var[c(12,13,14,15,16,17,18)])
env.var.stack2<-stack(env.var[c(19,20,21,22,23,24)])
env.var.stack3<-stack(env.var[c(1)])
env.var.stack4<-stack(env.var[c(25)])
env.var.stack5<-stack(env.var[c(2)])




names(data)
data1<-extract(env.var.stack1, data[,11:10])
data2<-extract(env.var.stack2, data[,11:10])
data3<-extract(env.var.stack3, data[,11:10])
data4<-extract(env.var.stack4, data[,11:10])
data5<-extract(env.var.stack5, data[,11:10])
env.var.stack5<-stack(env.var[c(1,19,21,23,28,30,40,42,44,46,48,50,52,54,56,58,60)])


data3.1<-extract(env.var.stack3, data[,11:10], buffer=30000, na.rm=TRUE, df=TRUE)
data4.1<-extract(env.var.stack4, data[,11:10], buffer=30000, na.rm=TRUE, df=TRUE)
data5.1<-extract(env.var.stack5, data[,11:10], buffer=30000, na.rm=TRUE, df=TRUE)


data.env<-cbind(data,data1,data2,data3,data4,data5)
write.csv(data.env,"~/Documents/UQ_Documents/Projects/GlobalCorals/CoralData/data.env.csv")




