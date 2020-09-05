###########################################################

# Arith methods
setMethod("Arith", c(e1="RasterArray", e2="numeric"), 
	definition=function(e1,e2){
		e1@stack <- raster::stack(methods::callGeneric(e1@stack, e2))
		e1
	}
)

setMethod("Arith", c(e2="RasterArray", e1="numeric"), 
	definition=function(e1,e2){
		e2@stack <- raster::stack(methods::callGeneric(e2@stack, e1))
		e2
	}
)

###########################################################
# Compare method

setMethod("Compare", c(e1="RasterArray", e2="RasterLayer"), 
	definition=function(e1,e2){
		e1@stack <- raster::stack(methods::callGeneric(e1@stack, e2))
		e1
	}
)

###########################################################
# Math method
setMethod("Math", c(x="RasterArray"), 
	definition=function(x){
		x@stack <- raster::stack(methods::callGeneric(x@stack))
		x
	}
)

###########################################################
# Math2 method
setMethod("Math2", signature=c(x="RasterArray"), 
	definition=function(x, digits){
		op=.Generic[[1]]
		switch(op,
			round = return({
					if(missing(digits)) digits <- 0
					x@stack <- raster::stack(round(x@stack,digits))
					x
				}),
			signif = return({
					if(missing(digits)) digits <- 6
					x@stack <- raster::stack(signif(x@stack,digits))
					x
				})
			
		)
	}
)

setMethod("Summary", c(x="RasterArray"), 
	definition=function(x,..., na.rm=FALSE){
		op<-.Generic[[1]]
	#	if(op=="range"){

	#	}else{
			methods::callGeneric(x@stack,..., na.rm=na.rm)	
	#	}
		
	}
)
