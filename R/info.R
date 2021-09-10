#' Retrieve citation of data object
#' 
#' The function prints or returns the citation string of a chosen object/item.
#'
#' The function is intended to be updated to handle BibTEX entries.
#' 
#' @param dat (\code{character}) Object downloaded with \code{\link{fetch}} or the database identifier string.
#' @param var (\code{character}) In case \code{dat} is \code{character}, the variable identifier.
#' @param ver (\code{character}) In case \code{dat} is \code{character}, the version identifier.
#' @param print (\code{logical}) Should the citations be printed to the console, or returned as a \code{character} vector.
#' @param prefix (\code{character}) In case the output is printed on the console. Use this to include a prefix before every entry.
#' 
#' @export
reference <- function(dat, var=NULL, ver=NULL, print=TRUE, prefix=""){
	# get the appropriate reference string
	if(is.chronosphere(dat)){
		# get the attributes
		refString <- attributes(dat)$chronosphere$reference
	}else{
		if("character"%in%class(dat)){
			# download a copy of the dataset
			download <- datasets(dat)

			if(!is.null(var)){
				download <- download[download$var%in%var, ]
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

#'Documentation page of a variable
#'
#'This is a temporary function that takes the user to the Evolv-ED blog.
#'
#' @param dat (\code{character}) Object downloaded with \code{\link{fetch}} or the database identifier string.
#' @param var (\code{character}) In case \code{dat} is \code{character}, the variable identifier.
#'
#' @export
info <- function(dat, var){
	# get the appropriate reference string
	if(is.chronosphere(dat)){
		# get the attributes
		infoURL <- attributes(dat)$chronosphere$info
	}else{
		if("character"%in%class(dat)){
			# download a copy of the dataset
			download <- datasets(dat)

			if(!is.null(var)){
				download <- download[download$var%in%var, ]
			}
			
			# the reference string
			infoURL <- unique(download$citation)
		}

	}
	browseURL(infoURL)
	
}

# import browseURL from utils

