#' @export rbind.SpatialArray
#' @exportS3Method rbind SpatialArray
#' @rdname bind-methods
cbind.SpatialArray<-function(...){
	listArg <- list(...)
	finRA <- listArg[[1]]
	for(i in 2:length(listArg)){
		finRA<-cbind2(finRA, listArg[[i]])
	}
	return(finRA)
}

#' @export rbind.SpatialArray
#' @exportS3Method rbind SpatialArray
#' @rdname bind-methods
rbind.SpatialArray<-function(...){
	listArg <- list(...)
	finRA <- listArg[[1]]
	for(i in 2:length(listArg)){
		finRA<-rbind2(finRA, listArg[[i]])
	}
	return(finRA)
}

