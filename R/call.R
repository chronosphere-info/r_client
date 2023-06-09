# function to construct a chronosphere call
ChronoCall <- function(dat, var, ver, res, ext,class, datadir, verbose=FALSE, expr=FALSE,...){
	# construct a function call
	theCall<-paste0('fetch(',
		'dat="', dat, '"')

	# add the optional arguments
	if(!is.null(var)){
		if(length(var)==1){
			theCall <- paste0(theCall, ", var=\"", var,"\"")
		}else{
			theCall <- paste0(theCall, ", var=c(\"", paste(var, collapse="\", \""),"\")")
		}
	}
	
	if(!is.null(ver)){
		if(length(ver)==1){
			theCall <- paste0(theCall, ", ver=\"", ver,"\"")
		}else{
			theCall <- paste0(theCall, ", ver=c(\"", paste(ver, collapse="\", \""),"\")")
		}

	}

	if(!is.null(res)){
		if(length(res)==1){
			theCall <- paste0(theCall, ", res=\"", res,"\"")	
		}else{
			theCall <- paste0(theCall, ", res=c(\"", paste(res, collapse="\", \""),"\")")
		}
	}

	if(!is.null(ext)){
		if(length(ext)==1){
			theCall <- paste0(theCall, ", ext=\"", ext,"\"")	
		}else{
			theCall <- paste0(theCall, ", ext=c(\"", paste(ext, collapse="\", \""),"\")")
		}
	}

	if(!is.null(class)){
		if(length(class)==1){
			theCall <- paste0(theCall, ", class=\"", class,"\"")	
		}else{
			theCall <- paste0(theCall, ", class=c(\"", paste(class, collapse="\", \""),"\")")
		}
	}

	if(!is.null(datadir)) theCall <- paste0(theCall, ", datadir=\"", datadir,"\"")
	if(!verbose) theCall <- paste0(theCall, ", verbose=", verbose)
	
	# still have to add variable specific arguments
	other <- list(...)
	if(length(other)!=0){
		for(i in 1:length(other)){
			if(is.character(other[[i]])){
				theCall <- paste0(theCall, ", ", names(other)[i],"=\"", other[[i]],"\"")
			}else{
				theCall <- paste0(theCall, ", ", names(other)[i],"=", other[[i]],"")
			}
		}
	}

	theCall <- paste0(theCall,")")

	if(!expr){
		message(theCall)
		return(NULL)
	}else{
		express <- parse(text=theCall)
		return(express)
	}
}
