context("dem, fetch(): downloading dems\n")

one <- fetch(dat="paleomap", var="dem", res=1, ver="20190719")


context("dem, fetch(): matches local copy")

# direct comparison is not possible because the link to the hard drive is hard coded.
test_that("match of proxy object", {
	# match of index vector
	expect_equal(one@index, arrayForm@index)
})

# get the stack
testStack <- one@stack

# get the stack directly from the arrayForm
stackReform <- arrayForm@stack

test_that("match of stack dimensions", {
	expect_equal(dim(testStack), dim(stackReform))
	expect_equal(extent(testStack), extent(stackReform))
	expect_equal(res(testStack), res(stackReform))
	expect_equal(projection(testStack), projection(stackReform))
	expect_equal(names(testStack), names(stackReform))
})

test_that("match of values", {
	# iterate
	for(i in 1:nlayers(stackReform)){
		expect_equal(values(testStack[[i]]), values(stackReform[[i]]))
	}
})

