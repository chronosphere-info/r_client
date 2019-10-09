# to be continued from main.R
# make sure to include
library(raster)
library(chronosphere)
library(testthat)

# Adam's path
# workdir <- "/media/adam/data/shared/Dropbox/WorkSpace/2019-07-08_chronosphere/chronosphere/tests/"
# workdir <- "D:/Dropbox/WorkSpace/2019-07-08_chronosphere/chronosphere/tests/"

# Nuss' path
# workdir <- here::here("tests")

# rel base
#setwd(workdir)

# load the DEMs manually from local storage
datPath <- "data/paleomap/1/dem/20190719"
allDem <- list.files(datPath)

# get the ages
one<- gsub("dem_", "", allDem)
age<- as.numeric(gsub("\\.nc", "", one))

# ordered vector
allDem<-allDem[order(age)]
# sort the ages 
age<- sort(age)

# read them in 
listForm <- list()
for(i in 1:length(allDem)){
	listForm[[i]] <- raster(file.path(datPath, allDem[i]))
	listForm[[i]]@data@names <- gsub("\\.nc", "", allDem)[i]
}
stackForm <- stack(listForm)

# proxy object
index <- 1:length(allDem)
names(index) <- age


arrayForm <- RasterArray(stack=stackForm, index=index)

# three links are tested for internal consistency
# 'stackForm', 'age' and 'arrayForm' which are the basic links to the generic


####################################################################
# Testing begins from here, organized by functions.

test <- "all"

if("fetch"%in%test | test=="all"){
	# specific test for fetching the data from the remote server
	test_dir("scripts/dem/fetch/", reporter=SummaryReporter)
}

if("matchtime"%in%test | test=="all"){
	# generic test for the algorithm of matchtime
	test_dir("scripts/generic/matchtime/", reporter=SummaryReporter)
}
