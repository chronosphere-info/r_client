library(chronosphere)
library(tinytest)

# create a directory
theDir <- "/home/adam/Desktop/chrontemp"

# make sure that it does not exist
system(paste0("rm -fr ", theDir))

# then create it
dir.create(theDir)




################################################################################
# datasets()

# SUBGUIDE
# do the initial download
expect_message(da <- datasets(datadir=theDir, verbose=TRUE, greetings=FALSE), pattern="Downloading registry tables.")
expect_equal("data.frame", class(da))

# expected files be present
expect_true("R"%in%list.files(theDir))

# the subguide should be there
expect_true("subguide.csv"%in%list.files(file.path(theDir,"R")))

# try to load the datasets from the place again, should not download anything
expect_message(da2 <- datasets(datadir=theDir, verbose=TRUE, greetings=FALSE), pattern="Found downloaded registry tables.")

# the results should be identical
expect_equal(da, da2)


# SUBGUIDE
# do the initial download
expect_message(daMaster <- datasets(datadir=theDir, verbose=TRUE, greetings=FALSE, master=TRUE), pattern="Downloading registry tables.")

# expect master to be present
expect_true("submaster.csv"%in%list.files(file.path(theDir,"R")))

# reload submaster
expect_message(daMaster2 <- datasets(datadir=theDir, verbose=TRUE, greetings=FALSE, master=TRUE), pattern="Found downloaded registry tables.")

# the results should be identical
expect_equal(daMaster, daMaster2)

# the guide is not the submaster
expect_false(nrow(daMaster)==nrow(da))


################################################################################
# fetch()

# check normal download
expect_silent(red <- fetch("SOM-reddin-sensitivity", verbose=FALSE))

# Object 1. - download regularly
expect_message(red2 <- fetch("SOM-reddin-sensitivity", datadir=theDir, verbose=TRUE), pattern="^.*Downloading data file.*$")

# should be the same
expect_equivalent(red, red2)

# Object 1. - should be already local
expect_message(red3 <- fetch("SOM-reddin-sensitivity", datadir=theDir, verbose=TRUE), pattern="^.*Loading downloaded data file.*$")

# Object 2. should be downloaded without problems
expect_message(ne <- fetch("NaturalEarth", datadir=theDir, verbose=TRUE), pattern="^.*Downloading data file.*$")

# repeated attempt should come from the hard drive
expect_message(ne2 <- fetch("NaturalEarth", datadir=theDir, verbose=TRUE), pattern="^.*Loading downloaded data file.*$")





## remote <- chronosphere:::remote
## timeout <- chronosphere:::timeout
## registers <- chronosphere:::registers
## dat <- NULL
## datadir<- theDir
## verbose <- FALSE
## master <- FALSE
## greetings <- TRUE
## all <- FALSE
