#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="RasterArray", value="RasterLayer"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j, value=value, ...)
		return(x)
	}
)


