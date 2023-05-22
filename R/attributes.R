# Function to prepare an attributes list 
ChronoAttributes <- function(dat, details,...){

	# general attributes
	baseList <- list(dat=details$dat, var=details$var, res=details$resolution, ver=details$ver, datafile=details$datafile, item=details$itemID)

	# reference of the archive
	baseList$reference <- unlist(strsplit(details$text, "\\|&\\|"))

	# the bibtex
	if(!is.na(details$bibtex)){
		baseList$bibtex <- unlist(strsplit(details$bibtex, "\\|&\\|"))

		# replace placeholder with newlines
		baseList$bibtex <- gsub("\\|-&-\\|", "\n",baseList$bibtex)
	}else{
		baseList$bibtex <- NULL
	}
	
	# when was the archive downloaded
	baseList$downloadDate <- Sys.time()

	# when was the archive accessed?
	baseList$publishDate <- details$dataDatePublished

	# original version
	baseList$infoURL <- details$infoURL

	# API call
	baseList$API <- details$API

	baseList$additional <- list(...)


	if(is.null(baseList$info)){
		baseList$info <- "https://www.chronosphere-portal.org/"

	}
	return(baseList)
}
