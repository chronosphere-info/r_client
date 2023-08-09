#' Retrieve citation of data object
#' 
#' The function prints or returns the citation string of a chosen object/item.
#'
#' @param src (\code{characater}) Object downloaded with \code{\link{fetch}} or the source identifier string.
#' @param bibtex (\code{logical}) Should a bibtex be printed/returned? 
#' @param ser (\code{character}) In case \code{src} is \code{character}, the series identifier.
#' @param ver (\code{character}) In case \code{src} is \code{character}, the version identifier.
#' @param print (\code{logical}) Should the citations be printed to the console, or returned as a \code{character} vector.
#' @param prefix (\code{characater}) In case the output is printed on the console. Use this to include a prefix before every entry.
#' 
#' @export
#' @return By default the function has no return value. If \code{print=FALSE}, the function will return a reference character string.
#' @examples
#' # A locally-present object, in package's directory
#' one <- fetch(src="SOM-zaffos-fragmentation",
#'   datadir=system.file("extdata", package="chronosphere"))
#' # its reference
#' reference(one)
reference <- function(src, bibtex=FALSE, ser=NULL, ver=NULL, print=TRUE, prefix=""){
	# get the appropriate refernce string
	if(is.chronosphere(src)){
		if(bibtex){

		}else{
			# get the attributes
			refString <- attributes(src)$chronosphere$reference
		}
	}else{
		if("character"%in%class(src)){
			# download a copy of the dataset
			download <- datasets(src)

			if(!is.null(ser)){
				download <- download[download$ser%in%ser, ]
			}
			if(!is.null(ver)){
				download <- download[download$ver%in%ver, ]
			}
			# the reference string
			refString <- unique(download$citation)
		}

	}
	

	allRefs <- unlist(strsplit(refString, "%and%"))
	# should the output be printed or returned
	if(print){
		for(i in 1:length(allRefs)) message(paste0(prefix, allRefs[i], "\n"))
	}else{
		return(allRefs)
	}
}

#'Documentation page of a series (Remote server under constructions)
#'
#' @param src (\code{character}) Object downloaded with \code{\link{fetch}} or the source identifier string.
#' @param ser (\code{character}) In case \code{src} is \code{character}, the series identifier.
#'
#' @export
#' @return The function has no return value.
info <- function(src, ser=NULL){
	# get the appropriate refernce string
	if(is.chronosphere(src)){
		# get the attributes
		infoURL <- attributes(src)$chronosphere$info
	}else{
		if("character"%in%class(src)){
			# download a copy of the dataset
			download <- datasets(src)

			if(!is.null(var)){
				download <- download[download$ser%in%ser, ]
			}
			
			# the reference string
			infoURL <- unique(download$citation)
		}

	}
	browseURL(infoURL)
	
}

# import browseURL from utils

