#' @rdname replacementSingle
setReplaceMethod(
	"[", 
	signature(x="SpatialArray", value="SpatialPoints"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j,..., value=value)
		return(x)
	}
)

#' @rdname replacementSingle
setReplaceMethod(
	"[", 
	signature(x="SpatialArray", value="SpatialPointsDataFrame"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j,..., value=value)
		return(x)
	}
)

#' @rdname replacementSingle
setReplaceMethod(
	"[", 
	signature(x="SpatialArray", value="SpatialLines"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j,..., value=value)
		return(x)
	}
)

#' @rdname replacementSingle
setReplaceMethod(
	"[", 
	signature(x="SpatialArray", value="SpatialLinesDataFrame"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j,..., value=value)
		return(x)
	}
)

#' @rdname replacementSingle
setReplaceMethod(
	"[", 
	signature(x="SpatialArray", value="SpatialPolygons"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j,..., value=value)
		return(x)
	}
)

#' @rdname replacementSingle
setReplaceMethod(
	"[", 
	signature(x="SpatialArray", value="SpatialPolygonsDataFrame"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j,..., value=value)
		return(x)
	}
)
