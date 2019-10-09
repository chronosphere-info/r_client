# make sure to include
library(raster)
library(chronosphere)
library(testthat)

# Adam's path
workdir <- "/media/adam/data/shared/Dropbox/WorkSpace/2019-07-08_chronosphere/chronosphere/tests/"
#workdir <- "D:/Dropbox/WorkSpace/2019-07-08_chronosphere/chronosphere/tests/"

# Nuss' path
# workdir <- here::here("tests")

# rel base
setwd(workdir)

source("scripts/dem/testDEM.R")
