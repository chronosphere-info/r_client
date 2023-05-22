library(tinytest)
expect_error(configure(curl=4))
expect_error(configure(curl=NA))
expect_error(configure(curl=c(TRUE, FALSE)))

# check whether it work
if(requireNamespace("curl", quietly=TRUE)){
	configure(curl=TRUE)
	expect_true(chronosphere2:::curl)
}
# this should give an error
if(!requireNamespace("curl", quietly=FALSE)){
	expect_error(configure(curl=TRUE))
}
# reverting should work all the time
configure(curl=FALSE)
expect_false(chronosphere2:::curl)
