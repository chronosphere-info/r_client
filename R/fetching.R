#' Data fetching
#' 
#' Function to download and attach variables in the \code{chronosphere} package
#' 
#' Use the function \code{\link{datasets}} to find available variables.
#' @param dat (\code{character}) The dataset to get variables from.
#' @param var (\code{character}) Vector of variable names to get.
#' @param res (\code{character} or \code{numeric}) The resolution of raster layers. This has to be the same for all RasterLayers that make up the variable.
#' @param ver (\code{character}) The version of the variable. Defaults to \code{NULL}, which will download the latest available version. We have to create a data table, which should be part of the package. This has to be searched for valid argument combinations. Right this is just a folder with a date.
#' @param class (\code{character}) Class of the returned object, if not the default.
#' @param ext (\code{character}) File extension of the used data file.
#' @param item (\code{numeric}) The item ID that is to be downloaded. This setting overrides all other identifiers.
#' @param datadir (\code{character}) Directory where downloaded files are kept. Individual layers will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#' @param verbose (\code{logical}) Should console feedback during download be displayed?
#' @param call (\code{logical}) If set to \code{TRUE} the function call is returned instead of the object. 
#' @param call.expr (\code{logical}) If \code{call} is set to \code{TRUE}, then should the call be returned as an \code{expression} (\code{TRUE}) or a message (\code{FALSE})?
#' @param attach (\code{logical}) If the item has required packages, should these be attached?
#' @param ... Arguments passed to variable-specific loading functions.
#' @examples
#' # An actual download call
#' # a <- fetch(dat="paleomap", var="dem")
#' # call repetition
#' fetch(dat="paleomap", var="dem", call=TRUE)
#' # A locally-present object, in package's directory
#' a <- fetch(dat="paleomap", var="model", 
#'   datadir=system.file("extdata", package="chronosphere"))
#' @export
#' @return An object that matches the 'type' field of the varibles in the output of the \code{\link{datasets}} function.
fetch <- function(dat=NULL, var=NULL, ver=NULL, res=NULL, ext=NULL, class=NULL, item=NULL,  datadir=NULL, verbose=TRUE, call=FALSE, call.expr=FALSE, attach=TRUE, ...){
	## dat="pbdb"
	## var="baseref"
	## ver=NULL
	## res=NULL
	## ext=NULL
	## class=NULL
	## item=NULL
	## datadir=NULL
	## verbose=TRUE
	## call=FALSE
	## call.expr=FALSE
	## attach=TRUE
	

	# fetch given an existing chronosphere object
	if(is.chronosphere(dat)){
		# force expression output of call reproduction if object should be downlaoded
		if(!call) call.expr <- TRUE

		# return call that can be used to replicate download
		att <- attributes(dat)$chronosphere

		# construct funtcion call
		argList <- list(
			dat=att$dat,
			var=att$var,
			ver=att$ver,
			res=att$res, 
			ext=att$ext, 
			class=att$class, 
			item=att$item, 
			datadir=att$datadir,
			verbose=FALSE,
			expr=call.expr)

		# extra arguments
		argList <- c(argList, att$additional)
		theCall <- do.call("ChronoCall", argList)

		if(!call){
			# re-download - recursive call to fetch()
			output <- eval(theCall)
		}else{
			output <- theCall
		}
		
		# return if it is an expression or an actually downloaded object
		if(!is.null(output)) return(output)


	# regular fetch given dataset character identifiers
	}else{
		if(!is.character(dat)) stop("Invalid 'dat' argument.")
		# return a call
		if(call){
			# construct function call
			theCall <- ChronoCall(dat, var, ver, res, ext, class, item, datadir, verbose, expr=call.expr,...)

			# return if it is an expression
			if(!is.null(theCall)) return(theCall)

		# do an actual fetch
		}else{
			# get the register of the dat
			if(length(dat)>1) stop("Only one dataset can be accessed in a single download call.")

			if(verbose){
				regs <- paste0(
"\n------------------------------------------------------------
Accessing chronos registry tables.
------------------------------------------------------------\n")
				message(regs)
			}
			# get the remote server data, or read it from hard drive!
			register <- datasets(dat=dat, datadir=datadir, verbose=verbose)

			# find the item in the register
			itemDetails <- FindItem(register, var=var, ver=ver, res=res, ext=ext, 
				class=class, item=item, datadir=datadir, verbose=verbose, ...)

			if(verbose){
				dets <- paste0(
"\n------------------------------------------------------------
Accessing item no. " ,itemDetails$itemID ,  ", dat:", itemDetails$dat, ", var: ", itemDetails$var, ", ver: ", itemDetails$ver, ".
------------------------------------------------------------\n")
				message(dets)
			}

			
			# execute the download and load the item 
			item <- DownloadItem(
				details=itemDetails,
				datadir = datadir,
				verbose=verbose,
				attach=attach, 
				...
			)

			# write the chronosphere attributes to the downloaded object
			attributes(item)$chronosphere <- ChronoAttributes(dat=dat, details=itemDetails, ...)

			# display citations
			if(verbose){
				message("
If you use the data in publications, please cite its
reference(s), as well as that of the 'chronosphere' project.\n")

				# print the reference
				reference(item, bibtex=FALSE, print=TRUE)
			}
	
			# return the item
			return(item)
		}

	}

}







# Actual fetch v3. -this function connects to the repo or loads the downloaded variable
# function to look up the item number
# param citation used to turn of citation display for recursive case
FindItem <- function(register, var=NULL, ver=NULL, res=NULL, ext=NULL, class=NULL, item=NULL, datadir=NULL, verbose=TRUE, citation=TRUE, ...){
	
	# item-based finding (still needs dat for efficient lookup)
	if(!is.null(item)){
		# the item number is given directly
		if(length(item)>1) stop("Only one item can be accessed in a single download call.")

		# get the necessary details for the item
		index <- which(register$itemID==item)
		if(length(index)==0){
			stop("The selected item does not exist in the subset.")
		}else{
			if(length(index)>1) stop("Oops, this should not happen.")

			# the item's information, which will be returned
			details<- item[index, ]
		}

	# coordinate-based lookup
	}else{

		########################################
		# A. Mandatory data
		# A1. Dat - already done
		# A2. Variable
		# Need the the default variable?
		if(is.null(var)){
			# make sure that this is available in the framework
			if(sum(register$defaultVariable)==0){
				stop("The default variable of the 'dat' is not available in R. ")
			# else: grab the parts that come from the defautl variable.
			}else{
				register <- register[register$defaultVariable, ]
				# save the variable for later
				var <- unique(register$var) 
			}
		}else{
			if(length(var)>1) stop("Only one variable can be accessed in a single download call.")
			# the index of the desired variables
			indVar <- which(register$var==var)
			if(length(indVar)==0){
				stop(paste0("The desired var '", var, "' is not available."))
			}else{
				# get the var-specific version 
				register <- register[indVar,]
			}
		}

		# A3. Version
		# The downloaded data must have a version
		# Default version: most up to date
		if(is.null(ver)){
			# select the most recent 
			dates <- unique(register$dataDatePublished)
			# field uses ISO dates, character sorting should be fine.
			mostRecent <- max(dates)
			# get the corresponding part
			register <- register[which(mostRecent==register$dataDatePublished), ]
		# 
		}else{
			if(length(ver)>1) stop("Only one version can be accessed in a single download call.")
			# the index of the desired variables
			indVer <- which(register$ver==ver)
			# is this present? 
			if(length(indVer)==0){
				stop(paste0("The desired version '", ver, "' is not available."))
			}else{
				# get the var-specific version 
				register <- register[indVer,]
			}
		}

		# B. Facultative coordinates
		# B1. the class
		# select the variable default
		if(is.null(class)){
			# the default class of the variable
			defClass <- unique(register$defaultClass)
			if(length(defClass)!=1) stop("Oops. This should not happen:\n more than one default class!")

			# where is the default class present? 
			bClass <- register$class==defClass

		# select the desired
		}else{
			if(length(class)>1) stop("Only one class can be returned in a single download call.")
			# the desired class is given 
			bClass <- register$class==class
			if(sum(bClass)==0) stop(paste0("The variable '", var, "' is not available as class '", class, "'."))
		}

		# B2. the resolution
		# default or simply NA 
		if(is.null(res)){
			# correct no entry
			bRes <- is.na(register$resolution)
			# in case this was simply just not given
			if(sum(bRes)==0){
				# look for those that are the default resolution
				# and overwrite
				bRes <- as.logical(register$resDefault)
			}
		# look for a specific resolution
		}else{
			if(length(res)>1) stop("Only one resolution can be accessed in a single download call.")
			# the correc resolution
			bRes <- res==register$resolution

			# missing values are not what we want
			bRes[is.na(bRes)] <- FALSE
			
		}

		# B3. the file extension	
		# no definition
		if(is.null(ext)){
			# properly not given - no data file required to run the bit
			bExt <- is.na(register$ext)

			# currently there is no protocol to select the file!
			# if there is nothing like this
			if(sum(bExt)==0){
				# this means that aynthing goes
				bExt <- rep(TRUE, length(bExt))
				
			}

		}else{
			if(length(ext)>1) stop("Only one file extensionbe accessed in a single download call.")

			# the correct file extension
			bExt <- ext==register$ext

			# missing values are not what we want
			bExt[is.na(bExt)] <- FALSE

		}


		# Where do these intersect? 
		match <- bExt & bClass & bRes
		index <- which(match)

		if(length(index)==0){
			stop("The specified item does not exist. ")
		}else{
			# multiple items with the same details exist
			if(length(index)>1) {
				# matching entries
				register <- register[index, ]

				# if there are multiple data files:
				native <- register$ext %in% c("rds", "rda", "RData", "Rdata")

				# missing values should not play
				native[is.na(native)] <- FALSE

				# if any of them are here:
				# select that!
				if(sum(native)>0){
					register <- register[native, ]
					
				# otherwise, select the one randomly
				}else{
					fileext <- unique(register$ext)[1]
					register <- register[which(fileext==register$ext), ]

				}
				# and then look for the itemversion!
				# look for the must up-to-date item
				recent <- max(register$itemVersion)

				# subset to the most recnet version
				index <- which(register$itemVersion==recent)

			}
			details <- register[index, ]
		}
		
	}
	# return the data necessary to downlaod the data
	return(details)
	
}


DownloadItem <- function(details, datadir=NULL, verbose=TRUE, attach=TRUE, ...){
#	details <- itemDetails
	# save timeout parameter from user's global options.
	original<- options()$timeout
	# set to chronosphere global 
	options(timeout=timeout)
	# ensure return to user's original on exit of function
	on.exit(expr=options(timeout=original))

	# basic defense
	if(nrow(details)!=1) stop("Multiple items found!")

	# rename for ease
	codefile <- details$codeFile
	datafile <- details$datafile
	primaryURL <- details$primaryURL	
	secondaryURL <- details$secondaryURL
	md5 <- details$fileMD5
	dat <- details$dat
	var <- details$var
	ver <- details$ver
	item <- details$itemID


	# in any case, the downloaded things will land in a specific directory
	# need to put this into an R- specific directory
	itemDir <- paste(dat, var, ver, item, sep="_")
	
	# do we need unzipping?
	zip <- FALSE
	# the actual file extension
	if(!is.null(datafile)){
		split <- strsplit(datafile, "\\.")[[1]]
		ext <- split[length(split)]
		# zipping? 
		if(ext=="zip"){
			zip <- TRUE
		}
	}

	# 1st case datadir does not exist
	if(is.null(datadir)){


		# create a temporary directory
		tempd <- tempdir()
		itemDirPath <- file.path(tempd, itemDir)

		# create a directory there, where everything will be downloaded
		dir.create(itemDirPath, showWarnings=FALSE)
	
		# the link to the code file
		codeURL <- paste0(remote, code, codefile)
		codePath <- file.path(itemDirPath, codefile)


		if(verbose){
			codes <- paste0(
"\n------------------------------------------------------------
Downloading import code.
------------------------------------------------------------\n")
			message(codes)
		}
		
		# download the code
		if(curl){
			curl::curl_download(
				codeURL,
				codePath, mode="wb", quiet=!verbose)
		}else{
			# download the code
			download.file(codeURL, codePath, mode="wb", quiet=!verbose)
		}

		# download the archive (if there is any!)
		if(!is.null(datafile)){
			# the path to the data file
			dataPath <- file.path(itemDirPath, datafile)

			if(verbose){
				codes <- paste0(
"\n------------------------------------------------------------
Downloading data file.
------------------------------------------------------------\n")
				message(codes)
			}

			if(curl){
				curl::curl_download(
					primaryURL,
					dataPath, mode="wb", quiet=!verbose)
			}else{
				# download the code
				download.file(primaryURL, dataPath, mode="wb", quiet=!verbose)
			}

			# do an MD5check
			dl_md5 <- tools::md5sum(dataPath)

			# compare!
			if(dl_md5!=md5){
				stop("MD5 checksum failed. Unexpected downloaded file.")
			}else{
				if(verbose) message("MD5 checksum passed.")
			}

			# is it compressed? - uncompress it!
			if(zip){
				unzip(dataPath, exdir=itemDirPath)	
			}

		}

	# there is a datadir
	}else{
		# add an R directory to this
		itemDir <- file.path("R", itemDir)

		#check whether the data need to be downloaded or not. 
		# list out the files
		all<-file.path("R", list.files(file.path("R", datadir)))
		
		# the full path to the directory
		itemDirPath <- file.path(datadir, itemDir)
		# is the directory present? - if not, create it! 	
		if(!any(all==itemDir)){

			# create a directory there, where everything will be downloaded
			suppressWarnings(dir.create(itemDirPath))
		}

		# check whether the files are actually there!
		all<- list.files(itemDirPath)

		# the code will be available here!
		codePath <- file.path(itemDirPath, codefile)

		# the data will be available here!
		dataPath <- file.path(itemDirPath, datafile)

		# is the codefile already there? No: download!
		if(!any(all==codefile)){
			# the link to the code file
			codeURL <- paste0(remote, code, codefile)

			if(verbose){
				codes <- paste0(
"\n------------------------------------------------------------
Downloading import code.
------------------------------------------------------------\n")
				message(codes)
			}
			# do an actual download
			if(curl){
				curl::curl_download(
					codeURL,
					codePath, mode="wb", quiet=!verbose)
			}else{
				# download the code
				download.file(codeURL, codePath, mode="wb", quiet=!verbose)
			}
		}else{

			if(verbose){
				codes <- paste0(
"\n------------------------------------------------------------
Loading downloaded import code.
------------------------------------------------------------\n")
				message(codes)
			}
			
		}				
		
		# is the datafile already there
		if(!any(all==datafile)){

			if(verbose){
				codes <- paste0(
"\n------------------------------------------------------------
Downloading data file.
------------------------------------------------------------\n")
				message(codes)
			}
			# execute the download
			if(curl){
				curl::curl_download(
					primaryURL,
					dataPath, mode="wb", quiet=!verbose)
			}else{
				# download the code
				download.file(primaryURL, dataPath, mode="wb", quiet=!verbose)
			}

			# do an MD5check
			dl_md5 <- tools::md5sum(dataPath)
		}else{
			if(verbose){
				codes <- paste0(
"\n------------------------------------------------------------
Loading downloaded data file.
------------------------------------------------------------\n")
				message(codes)
			}
		}				

		# is it compressed? - uncompress it!
		if(zip){
			# create a temporary directory to unzip into
			tempd <- tempdir()
			# this will be the operating directory 
			itemDirPath <- file.path(tempd, itemDir)

			# make sure this is there!
			suppressWarnings(dir.create(itemDirPath))
			
			# unzip to temporary!
			unzip(dataPath, exdir=itemDirPath)	
		}

	}

	# load in the downloaded code
	source(codePath)

	# then invoke replaced function in package namespace
#	obj <- loadVar(dir=itemDirPath, verbose=verbose, ...)
	obj <- loadVar(dir=itemDirPath, verbose=verbose, attach=attach, ...)

	# if there was a temporary directory
	# 1. entire download was there
	# 2. archive had to be unzipped
	if(is.null(datadir) | zip ) {
		unlink(tempd)
	}

	# result
	return(obj)

}

# placeholder function in package namespace
loadVar <- function(dir, verbose, attach){
	stop("If this method is run and you see this, then you have encountered an error.")
}


