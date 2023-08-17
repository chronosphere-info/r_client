library(chronosphere)
library(tinytest)

################################################################################
# datasets()
# local example INCOMPLETE - does not connect to the internet

# the  total subguide
expect_message(ind <- datasets(datadir=system.file("extdata", package="chronosphere"), verbose=TRUE, greetings=FALSE),
	pattern="Found downloaded registry tables.")

# the zaffos data
expect_message(
	ind <- datasets(
		src="SOM-zaffos-fragmentation", 
		datadir=system.file("extdata", package="chronosphere"), 
		verbose=TRUE, greetings=FALSE),
	pattern="Found downloaded registry tables."
)


# one archive
# create temporary file
stdout <- vector('character')
con    <- textConnection('stdout', 'wr', local = TRUE)
sink(con, type = "message")
a <- fetch(src="SOM-zaffos-fragmentation",
  datadir=system.file("extdata", package="chronosphere"))
sink()
close(con)

# look for the presence of the found statements
expect_true(any(grepl("Found downloaded registry tables.",stdout)))
expect_true(any(grepl("Loading downloaded import code.",stdout)))
expect_true(any(grepl("Loading downloaded data file.",stdout)))


