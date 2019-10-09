
# test only if 
length(dim(arrayForm))==1

context("generic, RasterArray-subset-[, one dimension: logical, null value")

# select one particular value
emptySub<- rep(FALSE, length(listForm))


test_that("correct length, 0 value, drop=TRUE",{

})

test_that("correct length, 0 value, drop=FALSE",{
	nullArr <- arrayForm[emptySub, drop=FALSE]
	expect_s4_class(nullArr,"RasterArray") # this means it is a rasterarry
	expect_equal(unstack(nullArr@stack), list())
	# weird exception - named integer with length 0
	expect_equal(nullArr@index, firstArr@index[0])
})


#######################################################
context("generic, RasterArray-subset-[, one dimension: logical, one value")

# subsetting masks
firstSub <- emptySub
firstSub[1] <- TRUE

lastSub <- emptySub
lastSub[length(lastSub)] <- TRUE

# script only works for length>3!!!!
midSub <- emptySub
midSub[sample(1:(length(lastSub)-2),1)] <- TRUE

shortSub <- firstSub[-length(firstSub)]
longSub <- c(firstSub, TRUE)

omitz <- function(rlayer){
	rlayer@z<-list()
	rlayer
}


testhat("correct length, one value, drop=TRUE", {
	# default properly set
	expect_equal(arrayForm[firstSub], arrayForm[firstSub, drop=TRUE])
	
	# layers are equal - besides the weird @z slot
	expect_equal(arrayForm[firstSub], omitz(listForm[[which(firstSub)]]))
	expect_equal(arrayForm[lastSub], omitz(listForm[[which(lastSub)]]))
	expect_equal(arrayForm[midSub], omitz(listForm[[which(midSub)]]))

})



testhat("correct length, one value, drop=FALSE", {
	# the subsets
	firstArr <- arrayForm[firstSub, drop=FALSE]
	lastArr <- arrayForm[lastSub, drop=FALSE]
	midArr <- arrayForm[midSub, drop=FALSE]
	
	# classes properly set  
	expect_s4_class(firstArr,"RasterArray")
	expect_s4_class(lastArr,"RasterArray")
	expect_s4_class(midArr,"RasterArray")
	
	# check the equality of RasterLayers
	expect_equal(unstack(firstArr@stack)[[1]], listForm[[which(firstSub)]])
	expect_equal(unstack(lastArr@stack)[[1]], listForm[[which(lastSub)]])
	expect_equal(unstack(midArr@stack)[[1]], listForm[[which(midSub)]])
	
	# check correct index
	expect_equal(as.numeric(firstArr@index), 1)
	expect_equal(as.numeric(lastArr@index), 1)
	expect_equal(as.numeric(midArr@index), 1)

	# check index names
	expect_equal(as.numeric(names(firstArr@index)), age[firstSub])
	expect_equal(as.numeric(names(lastArr@index)), age[lastSub])
	expect_equal(as.numeric(names(midArr@index)), age[midSub])
	
})

# select with NAs - these are the really weird ones. 

# copy NAs to subscripts
firstSubNA<- firstSub
lastSubNA <- lastSub
midSubNA <- midSub

# add NAs to the subscripts
firstSubNA[2] <-NA
lastSubNA[length(lastSubNA)-1] <-NA
midSubNA[which(midSubNA)+1] <- NA


########
context("generic, RasterArray-subset-[, one dimension: logical, multiple values")

firstTwo <- firstSub
firstTwo[2] <- TRUE

lastTwo <- lastSub
lastTwo[length(lastTwo)-1] <- TRUE

midTwo <- emptySub
midTwo[sample(1:(length(midTwo)-2),2, replace=FALSE)] <- TRUE

test_that("correct length, two values", {
	# the subsets
	firstArrTwo <- arrayForm[firstTwo, drop=FALSE]
	lastArrTwo <- arrayForm[lastTwo, drop=FALSE]
	midArrTwo <- arrayForm[midTwo, drop=FALSE]
	
	# properly set classes 
	expect_s4_class(firstArrTwo,"RasterArray")
	expect_s4_class(lastArrTwo,"RasterArray")
	expect_s4_class(midArrTwo,"RasterArray")

	# correct number of layers 
	expect_equal(nlayers(firstArrTwo), 2)
	expect_equal(nlayers(lastArrTwo), 2)
	expect_equal(nlayers(midArrTwo), 2)
	
	# check the equality of RasterLayers
	for(i in 1:nlayers(firstArrTwo)){
		expect_equal(unstack(firstArrTwo@stack)[[i]], listForm[[which(firstTwo)[i]]])
	}
	for(i in 1:nlayers(lastArrTwo)){
		expect_equal(unstack(lastArrTwo@stack)[[i]], listForm[[which(lastTwo)[i]]])
	}
	for(i in 1:nlayers(midArrTwo)){
		expect_equal(unstack(midArrTwo@stack)[[i]], listForm[[which(midTwo)[i]]])
	}

	# check correct index
	expect_equal(as.numeric(firstArrTwo@index), 1:nlayers(firstArrTwo))
	expect_equal(as.numeric(lastArrTwo@index),  1:nlayers(lastArrTwo))
	expect_equal(as.numeric(midArrTwo@index),  1:nlayers(midArrTwo))

	# check correct ages
	expect_equal(as.numeric(names(firstArrTwo)), age[firstTwo])
	expect_equal(as.numeric(names(lastArrTwo)), age[lastTwo])
	expect_equal(as.numeric(names(midArrTwo)), age[midTwo])


})




testhat("incorrect subscript length",{
	# produces normal subset with warning
	expect_warning(longArr<<-arrayForm[longSub]) 

	# produces normal subset with warning
	expect_warning(shortArr<<-arrayForm[shortSub]) 
})


# multiple subsetted version, resubsettable
