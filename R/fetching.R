# remote server 
remote <- "https://www.cnidaria.nat.fau.de/tersane/public/chronosphere/v2/"
userpwd <- NULL


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
#' @return A \code{data.frame} class object.
#' @examples
#' \donttest{
#' ind <- datasets()
#' View(ind)
#' }
#' @export
datasets <- function(dat=NULL, datadir=NULL, verbose=FALSE, master=FALSE){
		
	# dat tells you what to look for.
	if(is.null(dat)){
		# simple data table with dat/var combinations
		datfile <- "chronos.csv"
		if(master) datfile <- "master.csv"
	}else{
		# recursive call to see whether the dat entry is available
		tempdat <- datasets(datadir=datadir)
	
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

		tempLog <- tempfile()
		
		# you can set target file, won't change anything if there is nothing to download
		tempReg<- file.path(datadir, datfile)
		tempLog <- tempfile()
	# need to download but not saved
	}else{
		# temporary files
		tempReg <- tempfile()
		tempLog <- tempfile()

	}

	# go on with download
	if(download){
		# if you have to download and if it is not the simple table
		if(!is.null(dat)) datfile <- file.path(dat, datfile)
		# do the download
		if(is.null(userpwd)){
			download.file(paste(remote, "log.csv", sep = ""),tempLog, mode="wb", quiet=TRUE)
			download.file(paste(remote, datfile, sep = ""),tempReg, mode="wb", quiet=!verbose)
		}else{
			download.file(paste("ftp://", userpwd, "@",remote, "log.csv", sep = ""),tempLog, mode="wb", quiet=TRUE)
			download.file(paste("ftp://", userpwd, "@",remote, datfile, sep = ""),tempReg, mode="wb", quiet=!verbose)
		}	
		
		# read server log
		log <- read.csv(tempLog, sep=",", header=TRUE, stringsAsFactors=FALSE)

		# display message intended for people using this particular version
		pkgver <- sessionInfo()$otherPkgs$chronosphere$Version
		bLine <- pkgver==log$version
		currentMessage <- log$message[bLine]
		if(length(currentMessage)!=0) if(""!=currentMessage) warning(currentMessage)

		# and set return value
		ret <- read.csv(tempReg, sep=";", header=TRUE, stringsAsFactors=FALSE)

		# get rid of the  temporary file
		if(is.null(datadir)) unlink(tempReg)
		unlink(tempLog)
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
		theCall <- ChronoCall(att$dat, att$var, att$ver, att$res, att$datadir, FALSE, expr=call.expr)

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
		# return a call
		if(call){
			# construct funcion call
			theCall <- ChronoCall(dat, var, ver, res, datadir, verbose, expr=call.expr)

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
ChronoCall <- function(dat, var, ver, res, datadir, verbose=FALSE, expr=FALSE){
	# construct a function call
	theCall<-paste0('fetch(',
		'dat="', dat, '"')

	# add the optional arguments
	if(!is.null(var)) theCall <- paste0(theCall, ", var=\"", var,"\"")
	if(!is.null(ver)) theCall <- paste0(theCall, ", ver=\"", ver,"\"")
	if(!is.null(res)) theCall <- paste0(theCall, ", res=\"", res,"\"")
	if(!is.null(datadir)) theCall <- paste0(theCall, ", datadir=\"", datadir,"\"")
	if(!verbose) theCall <- paste0(theCall, ", verbose=", verbose)
	# still have to add ...
	theCall <- paste0(theCall,")")

	if(!expr){
		message(theCall)
		return(NULL)
	}else{
		express <- parse(text=theCall)
		return(express)
	}
}


# Actual fetch v2. -this function connects to the dataset or loads the downloaded variable
FetchVars <- function(dat, var=NULL, ver=NULL, res=NULL, datadir=NULL, verbose=TRUE,...){
	
	# only one should be allowed
	if(length(dat)>1) stop("Only one dataset can be accessed in a single download call.")

	# get the remote server data, or read it from hard drive!
	register <- datasets(dat=dat, datadir=datadir, verbose=verbose)
	
	# subset registry to dataset
	bDat <- register$dat==dat
	
	# select the default variable
	if(is.null(var)){
		var <- register[register$default_var, "var"]
	}
	
	# if one of the variables do not exist, then omit it
	present <- var%in%register$var

	# stop if not present
	if(any(!present)){
		if(length(present)==1) stop(paste("Variable '", var, "' does not exist.", sep=""))
		if(sum(present)>1) stop("The variables do not exist.")
		
		# some do, but some are missing
		warning(paste("The variable(s) '",paste(var[!present],collapse="', '"), "' does/do not exist at and is/are omitted.",  sep=""))
		var<-var[present]
	}

	# check whether the types are compatible
	if(!is.null(var)) bVar <- register$var%in%var else bVar <- rep(T, nrow(register))

	# which variable is which type?
	varType <- unique(register$type[bVar])
	
	# only one variable type is allowed!
	if(length(varType)>1){
		ret <- register[bVar, c("var", "type")]
		print(ret)
		stop("You can only download one variable type in a single download call.\nRepeat download with one type.")
	}

	
	# variable download has to be repeated for every variable
	# 1. DOWNLOAD a single variable - base case
	if(length(var)==1){

		register <- register[which(register$var==var),]

		# A. RESOLUTION LIMITING
		# Default resolution
		if(is.null(res)) res <- register$res[register$default_res]

		# only one version
		if(length(res)>1) stop("Only one resolution can be used in a single download call.")
		
		# again, limit registry - now to desired resolution
		register <- register[register[, "res"]==res, , drop=FALSE]
		if(nrow(register)==0) stop(paste0("The variable \'", var, "\' is not available at resolution ", res, "."))

		# B. VERSION LIMITING
		# Default version
		if(is.null(ver)){ 
			ver <- register$ver[register$default_ver]
		}

		# only one version
		if(length(ver)>1) stop("Only one version can be used in a single download call.")
		
		# again, limit registry - now to desired verson
		register <- register[register[, "ver"]==ver, , drop=FALSE]
		if(nrow(register)==0) stop(paste0("Version \'", ver, "\' of variable \'", var, "\' is not available at resolution ", res, "."))

		# after all this is done, there should be just one row in the table...
		if(nrow(register)!=1) stop("This should not have happened.")

		# do the actual download of this variable
		downloaded <- FetchArchive(dat=dat, var=var, ver=ver, res=res, datadir=datadir, 
			link=register$access_url, archive=register$archive_name, verbose=verbose,...)
	
	# 2. DOWNLOAD MULTIPLE VARIABLSE - recursive case
	}else{
		# subset the register to only look for var-specific part
		if(!is.null(var)){
			register <- register[which(register$var%in%var),]
		}
		# recursively download archives and bind them to a list.
		for(i in 1:length(var)){
			# all variables have to be present at the desired resolution
			stop(paste("The variable '", var[i], "' does not exist at the desired resolution (", res, "). ", sep=""))

			stop("Not yet!")

		}
		# process the recursive download if there is a method for binding it together
	}

	# write the chronosphere attributes to the downloaded object
	attributes(downloaded)$chronosphere<- ChronoAttributes(dat=dat, var=var, res=res, ver=ver, reg=register)
	

#	if(varType=="data.frame"){
#		# check for non-related input
#		if(!is.null(res)) warning("Argument 'res' is ignored for data.frame fetching.")
#		combined <- fetchDF(dat=dat, var=var, ver=ver, datadir=datadir, register=register, verbose=verbose)
#	}
#
#	if(varType=="platemodel"){
#		# check for non-related input
#		if(!is.null(res)) warning("Argument 'res' is ignored for plate model fetching.")
#		combined <- fetchModel(dat=dat, var=var, ver=ver, datadir=datadir, register=register, verbose=verbose)
#	}

	# display citations
	if(verbose){
		message("If you use the data in publications, please cite its\nreference, as well as that of the 'chronosphere' package.")
		for(i in 1:length(attributes(downloaded)$chronosphere$reference)){
			cat("\n")
			message(paste("-", attributes(downloaded)$chronosphere$reference[i]))
		}
	}
	
	return(downloaded)
	
}

# function to download and load the contents of an individual archive
FetchArchive <- function(dat, var, res, ver, archive, link, datadir=NULL, verbose=verbose,...){
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
				download.file(paste(remote, link,  sep = ""),pathToArchive, mode="wb", quiet=!verbose)
			}else{
				download.file(paste("ftp://", userpwd, "@",remote, link,  sep = ""),pathToArchive, mode="wb", quiet=!verbose)
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
			download.file(paste0(remote, link),temp, mode="wb", quiet=!verbose)
		}else{
			download.file(paste0("ftp://", userpwd, "@",remote, link,  sep = ""),temp, mode="wb", quiet=!verbose)
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
	varObj <- chronosphere:::loadVar(dat=dat, var=var, dir=varDir, ...)

	# 'get rid of' temporary directory
	unlink(tempd)

	# return object
	return(varObj)

}

# placeholder function in package namespace
loadVar <- function(variable, version, resChar,dir){
	stop("If this method is run and you see this, then you have encountered an error.")
}


# Function to prepare an attributes list 
ChronoAttributes <- function(dat=NULL, var=NULL, res=NULL, ver=NULL, reg=NULL){
	# general attributes
	baseList <- list(dat=dat, var=var, res=res, ver=ver)

	# reference of the archive
	baseList$reference <- unique(register$citation)
	
	# when was the archive downloaded
	baseList$downloadDate <- Sys.time()

	# original version
	baseList$info <- register$info

	if(is.null(baseList$info)){
		baseList$info <- "https://www.evolv-ed.net/"

	}
	return(baseList)
}






























# Raster-specific submodule of fetch()
fetchRemote <- function(dat, var, res=NULL, ver=NULL, datadir=NULL, register=register, verbose=TRUE,...){
	
	# default resolution used
	noRes <- FALSE
#	# subset the register to the resolution of interest
#	if(!is.null(res)){
#		
#	
#	# select the coarsest resolution
#	}else{
#		res <- max(register[, "res"])
#		# if res is NA, than the dataset has no resolution variable
#		if(is.na(res)) noRes <- TRUE
#	}
#
#	# check whether resolution is there, before the download
#	for(j in 1:length(var)){
#		
#	}

	# resolution variable for files
	resChar<-gsub("\\.","p", as.character(res))

	# variable versions
	if(is.null(ver)){
		verLong <- rep(NA, length(var))
	}else{
		# typical R reuse for one entry
		if(length(ver)==1){
			verLong <- rep(ver, length(var))
		}else{
			# otherwise version should be explicitly given for all variables 
			if(length(ver)!=length(var)) stop("Some variable versions were not provided. Use NULL to use the latest.")

			# coerce same format
			verLong <- ver
		}
		# if this passes, then the variables will be checked during the loop
	}

	citation <- NULL
	varObj <-list()
	# for all the variables
	for(j in 1:length(var)){
		# current version - NA to use default
		version <- verLong[j]

		# check structure database, whether the given verison is alright
		varReg<- register[register[,"var"]==var[j],, drop=FALSE]
		citation <-c(citation, varReg$citation)

		# no version number given for the variable
		if(is.na(version)){
			# select latest version 
			version <- varReg[order(varReg[,"date"], decreasing=TRUE)==1,"ver"]
		
		# version number is given 
		}else{
			if(!any(version==varReg[, "ver"])) stop(paste("Invalid variable version for ", var[j], sep=""))
		}
		
		# Check whether download is required or not
		if(noRes){
			archive <- paste(var[j],"_", version, ".zip", sep="")
		}else{
			# the name of the res_variable_ver-specific archive
			archive <- paste(resChar,"_",  var[j],"_", version, ".zip", sep="")
		}

		}

	# output - irst varialbe
	final <- varObj[[1]]
	
	# are there more?
	if(length(var)!=1){
		for(j in 2:length(var)){
			nex <- varObj[[j]]
			final<- cbind2(final, nex, deparse.level=-1)
		}
		colnames(final) <- var
	}

	citation <- unique(citation)
	return(list(final=final, citation=citation))
}
























##################################################################################
# DEPRECATED PART: REUSE in loadVar instances!


fetchModel <- function(dat, var, ver, datadir, register, verbose=TRUE){
	# unlikely to have multiple variables
	if(length(var)>1) stop("Only one data.frame type variable can be downloaded.")
	
	# check structure database, whether the given verison is alright
	if(is.null(var)){
		varReg <- register
		varName <- NULL
		varPath <- NULL
	}else{
		varReg<- register[register[,"var"]==var, ,drop=FALSE]
		varName <- paste(var, "_", sep="")
		varPath <- paste(var, "/", sep="")
	}

	citation <- unique(varReg$citation)

	# no version number given for the variable
	if(is.null(ver)){
		# select latest version 
		ver <- varReg[order(varReg[,"date"], decreasing=TRUE)==1,"ver"]
	
	# version number is given 
	}else{
		if(!any(ver==varReg[, "ver"])) stop(paste("Invalid variable version for ", var, sep=""))
	}
	
	# formatting
	format<-unique(varReg$format)
	
	# the name of the res_variable_ver-specific archive
	dir <- paste(dat, "_", varName, ver,  sep="")
	archive<- paste(dir, ".zip", sep="")
	
	# we need a temporary directory to store the extracted files until the end of the session
	tempd <- tempdir()

	# save the data for later?	 
	if(!is.null(datadir)){
		
		#check whether the data need to be downloaded or not. 
		all<-list.files(datadir)
		
		# target
		pathToFile<- file.path(datadir, archive)
			
		# is the archive not downloaded?
		# do a download
		if(!any(all==archive)){
			# download archive
			if(is.null(userpwd)){
				download.file(paste(remote, dat,"/",  varPath, archive,  sep = ""),pathToFile, mode="wb",quiet=!verbose)
			}else{
				download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  varPath, archive,  sep = ""),pathToFile, mode="wb",quiet=!verbose)
			}
		}
		# unzip it in temporary directory
			unzip(pathToFile, exdir=tempd)
		

	# must download
	}else{
		
		# temporary files
		temp <- tempfile()
	
		# download archive
		if(is.null(userpwd)){
			download.file(paste(remote, dat,"/",  varPath,archive,  sep = ""),temp, mode="wb",quiet=!verbose)
		}else{
			download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  varPath, archive,  sep = ""),temp, mode="wb",quiet=!verbose)
		}

		pathToFile<-file.path(temp, dir)

		# unzip it in temporary directory
		unzip(temp, exdir=tempd)
		
		# get rid of the archive
		unlink(temp)
	}
	
	# read the file in
	if(format=="mod"){
		# read in the file
		final <- platemodel(paste(tempd, "/",dir, "/", dir, ".", format, sep=""))
	}

	return(list(final=final, citation=citation))
}


# data.frame-specific submodule of fetch()
fetchDF <- function(dat, var, ver, datadir, register, verbose=TRUE){
	# unlikely to have multiple variables
	if(length(var)>1) stop("Only one data.frame type variable can be downloaded.")
	
	# check structure database, whether the given verison is alright
	if(is.null(var)){
		varReg <- register
		varName <- NULL
		varPath <- NULL
	}else{
		varReg<- register[register[,"var"]==var, ,drop=FALSE]
		varName <- paste(var, "_", sep="")
		varPath <- paste(var, "/", sep="")
	}

	citation <- unique(varReg$citation)

	# no version number given for the variable
	if(is.null(ver)){
		# select latest version 
		ver <- varReg[order(varReg[,"date"], decreasing=TRUE)==1,"ver"]
	
	# version number is given 
	}else{
		if(!any(ver==varReg[, "ver"])) stop(paste("Invalid variable version for ", var, sep=""))
	}
	
	# formatting
	format<-unique(varReg$format)
	
	# the name of the res_variable_ver-specific archive
	archive<- paste(dat, "_", varName, ver, ".", format, sep="")
	
	
	# save the data for later?	 
	if(!is.null(datadir)){
		
		#check whether the data need to be downloaded or not. 
		all<-list.files(datadir)
		
		# target
		pathToFile<- file.path(datadir, archive)
			
		# is the archive not downloaded?
		# do a download
		if(!any(all==archive)){
			# download archive
			if(is.null(userpwd)){
				download.file(paste(remote, dat,"/",  varPath, archive,  sep = ""),pathToFile, mode="wb",quiet=!verbose)
			}else{
				download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  varPath, archive,  sep = ""),pathToFile, mode="wb",quiet=!verbose)
			}
		}

	# must download
	}else{
		
		# temporary files
		temp <- tempfile()
	
		# download archive
		if(is.null(userpwd)){
			download.file(paste(remote, dat,"/",  varPath, archive,  sep = ""),temp, mode="wb",quiet=!verbose)
		}else{
			download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  varPath, archive,  sep = ""),temp, mode="wb",quiet=!verbose)
		}

		pathToFile<-temp
	}
	
	# read the file in
	if(format=="rds"){
		# read in the file
		if(verbose) cat("Reading downloaded file.\n")
		final <- readRDS(pathToFile)
	}

	if(format=="csv"){
		separator <- unique(varReg$separator)
		if(separator=="comma") sep <- ","
		if(separator=="semicolon") sep <- ";"
		final <- read.csv(pathToFile, header=TRUE, sep=sep, stringsAsFactors=FALSE)
	}
	return(list(final=final, citation=citation))
}

