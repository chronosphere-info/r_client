context("generic, matchtime(): basic functionality")


# ages to martch to
library(divDyn)
data(stages)
target <- stages$mid

# different types of input
ageNum <- age
ages <- names(arrayForm)

# do the process manually
manInd <- rep(NA, length(target))
for(i in 1:length(manInd)){
	abDiff <- abs(target[i]-ageNum)
	manInd[i] <- which(min(abDiff)==abDiff)[1]
}


test_that("numeric argument", {
	# should return indices
	expect_equal(manInd, matchtime(ageNum, target, index=TRUE))
	
	# should return names
	expect_equal(ageNum[manInd], matchtime(ageNum, target))
})

test_that("character method", {
	# should return indices
	expect_equal(manInd, matchtime(ages, target, index=TRUE))
	
	# should return names
	expect_equal(ages[manInd], matchtime(ages, target))
})

test_that("RasterArray method", {
	# index output
	expect_equal(manInd, matchtime(arrayForm, target, index=TRUE))

	# RasterArray output
	if(length(dim(arrayForm))==1){
		expect_equal(arrayForm[manInd], matchtime(arrayForm, target))
	}
})

