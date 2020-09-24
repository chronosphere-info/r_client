#' @method as.data.frame SpatialArray
#' @rdname as.data.frame-methods
#' @export 
as.data.frame.SpatialArray <- function(x, row.names=NULL, optional=FALSE,...){
	df <- as.data.frame(proxy(x))
	if(ncol(df)==1) colnames(df) <- "X0"
	if(!is.null(row.names)){
		rownames(df)<- row.names
	}
	if(optional){
		rownames(df) <- NULL
		colnames(df) <- NULL
	}
	return(df)
}
