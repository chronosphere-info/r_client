
# build from existing stack with existing index, or dimensions
#' @export SpatialArray
setMethod("initialize",signature="SpatialArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){
		# some defense for index
		if(is.null(dim)){ 
			if(class(stack)!="SpatialStack" & class(stack)!="character") stop("The 'stack' has to be a 'SpatialStack' - class object.")
			if(!is.numeric(index)) stop("The 'index' has to be a 'numeric' object.")
			
			# add the immmediate loading
			if("character"%in%class(stack)) stack <- SpatialStack(stack)

			# where were supposed to be NAs
			bNA <- is.na(index)

			if(any(index[!bNA]%%1!=0) | any(index[!bNA]<1)) stop("The 'index' has to contain positive integer values.")
			
			# the number of valid entries mismatch the number of layers
			if(sum(!bNA)!=nlayers(stack)) stop("You have to provide as many layers as many valid entries in index.")

			# reorder the stack
			noNAInd <- index[!bNA]
			newStack <- stack[[noNAInd]]

			# force index to be monotonous integer sequence
			newIndex <- index
			newIndex[] <- NA
			newIndex[!bNA] <- 1:nlayers(stack)

			# promote RasterLayer if that is the only Layer
			if(inherits(newStack, "Spatial")) newStack <- SpatialStack(newStack)

			# store final object
			.Object@index <- newIndex
			.Object@stack <- newStack
			
		}else{
			if(!is.numeric(dim)) stop("The 'dim' argument has to be a 'numeric' vector.")
			if(nlayers(stack)!=prod(dim, na.rm=TRUE)) warning("The number of layers in the does not equal the product of the 'dim' vector.")
			.Object@stack<- stack
			index <- array(1:nlayers(stack), dim=dim)
			# in case of reuse
			index[duplicated(as.numeric(index))] <- NA
			.Object@index<- index
			
			
		}
		return(.Object)
	}
)
