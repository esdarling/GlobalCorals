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



## example

#list all the nc data in a folder assuming there are n files in the dir
files=list.files(path='', pattern=nc)

#reading the furst nc in files
s=qss(files, 1)

##s is a raster stack file, to see the dimensions of s
s
# to plot one of the files in s time series
a=raster(s, 1)
plot(a)

##layer names in s
names(layernames)
