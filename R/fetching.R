# remote server 
remote <- "https://www.cnidaria.nat.fau.de/tersane/public/chronosphere/chrono-arch-2/"
userpwd <- NULL
checklog <- TRUE

#v2
#' Download a database extract from \code{chronosphere} remote server
#' 
#' The function will download a list of available data from the data repository
#' 
#' The function will download a single .csv file and attached it as a \code{data.frame}.
#' 
#' @param dat \code{character}. Dataset ID. If this is set to \code{NULL}, then a simplified list of availables variables will be downloaded, including all \code{dat} and \code{var} combinations. If \code{dat} is a valid dataset ID, then all accessible variables of a dataset are downloaded. 
#' @param datadir \code{character} Directory where the downloaded files are kept. Individual entries will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#' @param verbose \code{logical} Should console feedback during download be displayed?
#' @param master \code{logical} When \code{dat} is \code{NULL}, should the function download the master records file?
#' @param greetings \code{logical} When the function is invoked without arguments, it displays a message to keep new users informed about different versions and resolutions (even with \code{verbose=FALSE}). This argument turns this message off on demand.
#' @return A \code{data.frame} class object.
#' @examples
#' \donttest{
#' # available datasets and variables
#' ind <- datasets()
#' # all available versions and resolutions in dataset 'paleomap'
#' oneDat <- datasets("paleomap")
#' }
#' @export
datasets <- function(dat=NULL, datadir=NULL, verbose=FALSE, master=FALSE, greetings=TRUE){
		
	# dat tells you what to look for.
	if(is.null(dat)){
		# simple data table with dat/var combinations
		datfile <- "chronos.csv"
		if(master){
			datfile <- "master.csv"
		}else{
			if(greetings) message("Use datasets(dat = <dat>) to see available versions and resolutions.") 
		}
	}else{
		# recursive call to see whether the dat entry is available
		tempdat <- datasets(datadir=datadir, greetings=FALSE)
	
		if(!any(dat%in%tempdat$dat)) stop(paste0("The dat entry \'", dat, "\' was not found."))

		# full list of available variables in a given dataset - used by fetch()
		datfile <- paste0(dat, ".csv")
	}

	# if it does not exist in datadir, then 
	# by default, download the file
	download <- TRUE

	# a data directory is given
	if(!is.null(datadir)){
		# list all files
		all<-list.files(datadir)

		# do any of them match? 
		if(any(datfile==all)){
			# read it in
			ret <- read.csv(file.path(datadir, datfile), sep=",", header=TRUE, stringsAsFactors=FALSE)

			# structure is ok
			if(sum(c("dat", "var", "ver", "res")%in%colnames(ret))==4){
				download <- FALSE
			} # no? ->download
		} # no? ->download

		# you can set target file, won't change anything if there is nothing to download
		tempReg<- file.path(datadir, datfile)
		if(checklog) tempLog <- tempfile()
	# need to download but not saved
	}else{
		# temporary files
		tempReg <- tempfile()
		if(checklog) tempLog <- tempfile()

	}

	# go on with download
	if(download){
		# if you have to download and if it is not the simple table
		if(!is.null(dat)) datfile <- file.path(dat, datfile)
		# do the download
		if(is.null(userpwd)){
			if(checklog) download.file(paste(remote, "log.csv", sep = ""),tempLog, mode="wb", quiet=TRUE)
			download.file(paste(remote, datfile, sep = ""),tempReg, mode="wb", quiet=!verbose)
		}else{
			if(checklog) download.file(paste("ftp://", userpwd, "@",remote, "log.csv", sep = ""),tempLog, mode="wb", quiet=TRUE)
			download.file(paste("ftp://", userpwd, "@",remote, datfile, sep = ""),tempReg, mode="wb", quiet=!verbose)
		}	
		
	
		# check the server log.
		if(checklog){
			# read server log
			log <- read.csv(tempLog, sep=",", header=TRUE, stringsAsFactors=FALSE)

			# display message intended for people using this particular version
			pkgver <- sessionInfo()$otherPkgs$chronosphere$Version
			bLine <- pkgver==log$version
			currentMessage <- log$message[bLine]

			logok <- FALSE
			if(length(currentMessage)!=0){
				if(""!=currentMessage){
					warning(currentMessage)
				}else{
					logok <- TRUE
				}
			}else{
				logok <- TRUE
			}
			if(logok) assignInNamespace("checklog", FALSE, ns="chronosphere")
			unlink(tempLog)
		}
		
		# and set return value
		ret <- read.csv(tempReg, sep=";", header=TRUE, stringsAsFactors=FALSE)

		# get rid of the  temporary file
		if(is.null(datadir)) unlink(tempReg)
		
	}

	return(ret)
}


#' Data fetching
#' 
#' Function to download and attach variables in the \code{chronosphere} package
#' 
#' Use the function \code{\link{datasets}} to find available variables.
#' @param dat (\code{character}) The dataset to get variables from.
#' @param var (\code{character}) Vector of variable names to get.
#' @param res (\code{character} or \code{numeric}) The resolution of raster layers. This has to be the same for all RasterLayers that make up the variable.
#' @param ver (\code{character}) The version of the variable. Defaults to \code{NULL}, which will download the latest available version. We have to create a data table, which should be part of the package. This has to be searched for valid argument combinations. Right this is just a folder with a date.
#' @param datadir (\code{character}) Directory where downloaded files are kept. Individual layers will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#' @param verbose (\code{logical}) Should console feedback during download be displayed?
#' @param call (\code{logical}) If set to \code{TRUE} the function call is returned instead of the object. 
#' @param call.expr (\code{logical}) If \code{call} is set to \code{TRUE}, then should the call be returned as an \code{expression} (\code{TRUE}) or a message (\code{FALSE})?
#' @param ... Arguments passed to variable-specific loading functions.
#' @examples
#' \donttest{
#' 	a <- fetch(dat="paleomap", var="dem")
#' }
#' @export
#' @return An object that matches the 'type' field of the varibles in the output of the \code{\link{datasets}} function.
fetch <- function(dat, var=NULL, ver=NULL, res=NULL, datadir=NULL, verbose=TRUE, call=FALSE, call.expr=FALSE, ...){

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
			theCall <- ChronoCall(dat, var, ver, res, datadir, verbose, expr=call.expr,...)

			# return if it is an expression
			if(!is.null(theCall)) return(theCall)

		# do an actual fetch
		}else{
			return(FetchVars(dat=dat, var=var, ver=ver, res=res, datadir=datadir, verbose=verbose, ...))
		}

	}

}

# function to determine if an object was downloaded from the chronosphere
is.chronosphere<-function(x){
	!is.null(attributes(x)$chronosphere)
}


# function to construct a chronosphere call
ChronoCall <- function(dat, var, ver, res, datadir, verbose=FALSE, expr=FALSE,...){
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





# Actual fetch v2. -this function connects to the repo or loads the downloaded variable
# param citation used to turn of citation display for recursive case
FetchVars <- function(dat, var=NULL, ver=NULL, res=NULL, datadir=NULL, verbose=TRUE, register=NULL, citation=TRUE, ...){
	

	# only one should be allowed
	if(length(dat)>1) stop("Only one dataset can be accessed in a single download call.")

	# get the remote server data, or read it from hard drive!
	if(is.null(register)) register <- datasets(dat=dat, datadir=datadir, verbose=verbose)
	
	# subset registry to dataset
	bDat <- register$dat==dat
	
	# select the default variable
	if(is.null(var)){
		var <- unique(register[register$default_var, "var"])
	}
	
	# if one of the variables do not exist, then omit it
	present <- var%in%register$var

	# stop if not present
	if(any(!present)){
		if(length(present)==1) stop(paste("Variable '", var, "' does not exist.", sep=""))
		if(sum(present)>1) stop("The variables do not exist.")
		
		# some do, but some are missing
		warning(paste("The variable(s) '", paste(var[!present],collapse="', '"), "' does/do not exist at and is/are omitted.",  sep=""))
		var<-var[present]
	}

#	# check whether the types are compatible
#	if(!is.null(var)) bVar <- register$var%in%var else bVar <- rep(T, nrow(register))
#
#	# which variable is which type?
#	varType <- unique(register$type[bVar])
#	
#	# only one variable type is allowed!
#	if(length(varType)>1){
#		ret <- register[bVar, c("var", "type")]
#		print(ret)
#		stop("You can only download one variable type in a single download call.\nRepeat download with one type.")
#	}

	
	# variable download has to be repeated for every variable
	# 1. DOWNLOAD a single variable - base case
	if(length(var)==1){

		register <- register[which(register$var==var),]

		# A. RESOLUTION LIMITING
	#	if(!is.null(res)) if(is.na(res)) res <- "none"
		# Default resolution
		if(is.null(res)){
			res <- unique(register$res[register$default_res])
		}else{
			if(length(res)>1) warning("Multiple 'res' arguments detected, only the first will be used.")
			res <- res[1]
		}

		# only one resolution
		if(length(res)>1) stop("INTERNAL error: Only one resolution can be used in a single download call.")
		
		# again, limit registry - now to desired resolution
		register <- register[register[, "res"]==res, , drop=FALSE]
		if(nrow(register)==0) stop(paste0("The variable \'", var, "\' is not available at resolution ", res, "."))

		# B. VERSION LIMITING
		# Default version
		if(is.null(ver)){ 
			ver <- register$ver[register$default_ver]
		}else{
			if(length(ver)>1) warning("Multiple 'ver' arguments detected, only the first will be used.")
			ver <- ver[1]
		}

		# only one version
		if(length(ver)>1) stop("INTERNAL error: Only one version can be used in a single download call.")
		
		# again, limit registry - now to desired verson
		register <- register[register[, "ver"]==ver, , drop=FALSE]
		if(nrow(register)==0) stop(paste0("Version \'", ver, "\' of variable \'", var, "\' is not available at resolution ", ver, "."))

		# after all this is done, there should be just one row in the table...
		if(nrow(register)!=1) stop("This should not have happened.")

		# do the actual download of this variable
		downloaded <- FetchArchive(dat=dat, var=var, ver=ver, res=res, datadir=datadir, 
			link=register$access_url, archive=register$archive_name, verbose=verbose,...)
		
		# write the chronosphere attributes to the downloaded object
	
		attributes(downloaded)$chronosphere<- ChronoAttributes(dat=dat, var=var, res=res, ver=ver, reg=register, ...)
	

	# 2. DOWNLOAD MULTIPLE VARIABLSE - recursive case
	}else{
		# subset the register to only look for var-specific part
		register <- register[which(register$var%in%var),]
		
		# select the version
		# if not all versions are default
		if(!is.null(ver)){
			# if just one is given, assume it is the same all variables
			if(length(ver)==1) ver <- rep(ver, length(var))
			if(length(ver)!=length(var)) stop("You have to provide a single, or as many 'ver' entries, as many variables ('var'). ")
		}

		# select resolution
		if(!is.null(res)){
			# if just one is given, assume it is the same all variables
			if(length(res)==1) res <- rep(res, length(var))
			if(length(res)!=length(var)) stop("You have to provide a single, or as many 'res' entries, as many variables ('var'). ")
		}

		# entity
		theList <- list()
		# recursively download archives and bind them to a list.
		for(i in 1:length(var)){
			# all variables have to be present at the desired resolution
		#	stop(paste("The variable '", var[i], "' does not exist at the desired resolution (", res, "). ", sep=""))

			# recursive call to FetchVars
			theList[[i]] <- FetchVars(dat=dat, var=var[i], ver=ver[i], res=res[i], datadir=datadir, 
				register=register, verbose=verbose,citation=FALSE, ...)

		}
		names(theList) <- var
		
		# process the recursive download if there is a method for binding it together, otherwise pass it through.
		downloaded <- CombineVars(theList)
	}

	# display citations
	if(citation & verbose){
		message("If you use the data in publications, please cite its\nreference, as well as that of the 'chronosphere' package.")
		for(i in 1:length(attributes(downloaded)$chronosphere$reference)){
			cat("\n")
			message(paste("-", attributes(downloaded)$chronosphere$reference[i]))
		}
	}
	
	return(downloaded)
	
}

# function to download and load the contents of an individual archive
FetchArchive <- function(dat, var, res, ver, archive, link, datadir=NULL, verbose=TRUE,...){
	# we need a temporary directory to store the extracted files until the end of the session
	tempd <- tempdir()

	# save the data for later?	 
	if(!is.null(datadir)){

		#check whether the data need to be downloaded or not. 
		all<-list.files(datadir)
		
		# target
		pathToArchive<- file.path(datadir, archive)
			
		# is the archive not downloaded?
		# do a download
		if(!any(all==archive)){

			# download archive
			if(is.null(userpwd)){
				download.file(paste0(remote, link, "/", archive),pathToArchive, mode="wb", quiet=!verbose)
			}else{
				download.file(paste("ftp://", userpwd, "@",remote, link,"/", archive, sep = ""),pathToArchive, mode="wb", quiet=!verbose)
			}
		}

		# and unzip to a temporary directory
		unzip(pathToArchive, exdir=tempd)

	# must download
	}else{
		# temporary files
		temp <- tempfile()
	
		# download archive
		if(is.null(userpwd)){
			download.file(paste0(remote, link, "/", archive),temp, mode="wb", quiet=!verbose)
		}else{
			download.file(paste0("ftp://", userpwd, "@",remote, link,"/", archive,  sep = ""),temp, mode="wb", quiet=!verbose)
		}
		
		# unzip it in temporary directory
		unzip(temp, exdir=tempd)
	
		# get rid of the archive
		unlink(temp)
	}
	
	# variable directory
	varDir <- file.path(tempd, gsub(".zip", "", archive))

	# loading script - variable specific
	loadScript <- gsub(".zip", ".R", archive)

	# source the R file associated with the variable
	source(file.path(varDir, loadScript))

	# run the function that loads in the variable and save it
	varObj <- loadVar(dir=varDir, verbose=verbose, ...)

	# 'get rid of' temporary directory
	unlink(tempd)

	# return object
	return(varObj)

}

# placeholder function in package namespace
loadVar <- function(dir, verbose){
	stop("If this method is run and you see this, then you have encountered an error.")
}


# Function to prepare an attributes list 
ChronoAttributes <- function(dat=NULL, var=NULL, res=NULL, ver=NULL, reg=NULL,...){

	# general attributes
	baseList <- list(dat=dat, var=var, res=res, ver=ver)

	# reference of the archive
	baseList$reference <- unique(reg$citation)
	
	# when was the archive downloaded
	baseList$downloadDate <- Sys.time()

	# when was the archive accessed?
	baseList$accessDate <- reg$access_date

	# original version
	baseList$info <- reg$info

	baseList$additional <- list(...)


	if(is.null(baseList$info)){
		baseList$info <- "https://www.evolv-ed.net/"

	}
	return(baseList)
}

# function to combine RasterArray variables
CombineVars <- function(theList){
	# method 1. RasterArray combination
	classes <- unique(unlist(lapply(theList, class)))
	
	happened <- FALSE
	if(length(classes)==1){
		# RasterArray case
		if(classes=="RasterArray"){
			# vectors
			extents <- lapply(theList, extent)
			combined <-  lapply(theList, extent)
			xRes <- rep(NA, length(theList))
			yRes <- rep(NA, length(theList))
			xmin <- rep(NA, length(theList))
			xmax <- rep(NA, length(theList))
			ymin <- rep(NA, length(theList))
			ymax <- rep(NA, length(theList))
			crss <- rep(NA, length(theList))

			# iterate for every list item
			for(j in 1:length(theList)){
				ex <- extent(theList[[j]])
				re <- res(theList[[j]])

				xRes[j] <- re[1]
				yRes[j] <- re[2]
				xmin[j] <- ex[1]
				xmax[j] <- ex[2]
				ymin[j] <- ex[3]
				ymax[j] <- ex[4]
				crss[j] <- crs(theList[[j]])
			}
			# assume things will happen from now
			happened <- TRUE

			if(length(unique(xRes))!=1 | length(unique(yRes))!=1){
				warning("Resolutions mismatch, returning a list. ", call.=FALSE)
				happened <- FALSE
			}

			if(length(unique(xmin))!=1 | length(unique(ymin))!=1 | length(unique(xmax))!=1 | length(unique(ymax))!=1){
				warning("Extents mismatch, returning a list. ", call.=FALSE)
				happened <- FALSE
			}

			if(length(unique(crss))!=1){
				warning("CRSs mismatch, returning a list. ", call.=FALSE)
				happened <- FALSE
			}

			# things do happen!!
			if(happened){
				# first variable
				combined <- theList[[1]]

				for(j in 2:length(theList)){
					combined <- cbind(combined, theList[[j]])
				}
				colnames(combined) <- names(theList)
				
			}

		}
	}

	if(!happened){
		combined <- theList 
	}
		
	# write the chronoattributes - have to be a separate loop!
	attributes(combined)$chronosphere <- attributes(theList[[1]])$chronosphere

	for(j in 2:length(theList)){
		attributes(combined)$chronosphere$var <- c(attributes(combined)$chronosphere$var, attributes(theList[[j]])$chronosphere$var)
		attributes(combined)$chronosphere$res <- c(attributes(combined)$chronosphere$res, attributes(theList[[j]])$chronosphere$res)
		attributes(combined)$chronosphere$ver <- c(attributes(combined)$chronosphere$ver, attributes(theList[[j]])$chronosphere$ver)
		attributes(combined)$chronosphere$reference <- c(attributes(combined)$chronosphere$reference, attributes(theList[[j]])$chronosphere$reference)
		attributes(combined)$chronosphere$accessDate <- c(attributes(combined)$chronosphere$accessDate, attributes(theList[[j]])$chronosphere$accessDate)
		attributes(combined)$chronosphere$additional <- c(attributes(combined)$chronosphere$additional, attributes(theList[[j]])$chronosphere$additional)
		attributes(combined)$chronosphere$info <- c(attributes(combined)$chronosphere$info, attributes(theList[[j]])$chronosphere$info)
	}

	return(combined)
	

}

