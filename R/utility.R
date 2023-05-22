# function to determine if an object was downloaded from the chronosphere
is.chronosphere<-function(x){
	!is.null(attributes(x)$chronosphere)
}
