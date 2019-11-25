# remote server 
remote <- "https://www.cnidaria.nat.fau.de/tersane/public/chronosphere/"
userpwd <- NULL

#' Download a database extract from \code{chronosphere} remote server
#' 
#' The function will download a list of available data from the data repository
#' 
#' The function will download a single .csv file and attached it as a \code{data.frame}.
#' 
#' @param datadir Directory where the downloaded file 'reg.csv' file is kept. Individual layers will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#'
#' @param verbose Should console feedback during download be displayed?
#' @export
dataindex <- function(datadir=NULL, verbose=FALSE){
		
	# if it does not exist in datadir, then 
	# by default, download the file
	download <- TRUE

	# a data directory is given
	if(!is.null(datadir)){
		# list all files
		all<-list.files(datadir)

		# do any of them match? 
		if(any("reg.csv"==all)){
			# read it in
			ret <- read.csv(file.path(datadir, "reg.csv"), sep=",", header=TRUE, stringsAsFactors=FALSE)

			# structure is ok
			if(sum(c("dat", "var", "ver", "res")%in%colnames(ret))==4){
				download <- FALSE
			} # no? ->download
		} # no? ->download

		tempLog <- tempfile()
		
		# you can set target file, won't change anything if there is nothing to download
		temp<- file.path(datadir, "reg.csv")

	# need to download but not saved
	}else{
		# temporary files
		tempReg <- tempfile()
		tempLog <- tempfile()

	}

	# go on with download
	if(download){
		# do the download
		if(is.null(userpwd)){
			download.file(paste(remote, "log.csv", sep = ""),tempLog, mode="wb", quiet=T)
			download.file(paste(remote, "reg.csv", sep = ""),tempReg, mode="wb", quiet=!verbose)
		}else{
			download.file(paste("ftp://", userpwd, "@",remote, "log.csv", sep = ""),tempLog, mode="wb", quiet=TRUE)
			download.file(paste("ftp://", userpwd, "@",remote, "reg.csv", sep = ""),tempReg, mode="wb", quiet=!verbose)
		}	
		
		# read server log
		log <- read.csv(tempLog, sep=",", header=TRUE, stringsAsFactors=FALSE)

		# display message intended for people using this particular version
		pkgver <- sessionInfo()$otherPkgs$chronosphere$Version
		bLine <- pkgver==log$version
		currentMessage <- log$message[bLine]
		if(""!=currentMessage) warning(currentMessage)

		# and set return value
		ret <- read.csv(tempReg, sep=",", header=TRUE, stringsAsFactors=FALSE)

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
#' The function is implemented for a single variable, which can be downloaded and attached efficiently (dem). There are a lot of things to figure out.
#' Version 2.
#' @param dat The dataset to get variables from.
#' @param var Vector of variable names to get.
#' @param res The resolution of raster layers. This has to be the same for all RasterLayers that make up the variable.
#' @param ver The version of the variable. Defaults to \code{NULL}, which will download the latest available version. We have to create a data table, which should be part of the package. This has to be searched for valid argument combinations. Right this is just a folder with a date.
#' @param datadir Directory where downloaded files are kept. Individual layers will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#' @param verbose Should console feedback during download be displayed?
#' @examples
#' \donttest{
#' 	a <- fetch(dat="paleomap", var="dem")
#' }
#' @export
fetch <- function(dat, var=NULL, ver=NULL, res=1, datadir=NULL, verbose=TRUE){
	# get the remote server data, or read it from hard drive!
	register <- dataindex(datadir=datadir, verbose=verbose)
	
	# the data have to use the same resolution!!!
	if(length(dat)>1) stop("Only one dataset can be accessed in a single download call.")

	# subset registry to dataset
	bDat <- register$dat==dat
	
	if(any(bDat)){
		register <- register[bDat, ]
	}else{
		stop(paste("The dataset '", dat, "' does not exist." , sep=""))
	}
	
	# if one of the variables do not exist, then omit it
	present <- var%in%register$var

	# stop if not present
	if(any(!present)){
		if(length(present)==1) stop(paste("Variable '", var, "' does not exist.", sep=""))
		if(sum(present)==0) stop("The variables do not exist.")
		
		# some do, but some are missing
		warning(paste("The variable(s) '",paste(var[!present],collapse="', '"), "' do not exist at and is/are omitted.",  sep=""))
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

	# subset the register to only look for var-specific part
	if(!is.null(var)){
		register <- register[which(register$var%in%var),]
	}
	

	# method dispatch
	if(varType=="raster"){
		combined <- fetchRaster(dat=dat, var=var, ver=ver, res=res, datadir=datadir, register=register, verbose=verbose)
	}

	if(varType=="data.frame"){
		# check for non-related input
		if(res!=1) warning("Argument 'res' is ignored for data.frame fetching.")
		combined <- fetchDF(dat=dat, var=var, ver=ver, datadir=datadir, register=register, verbose=verbose)
	}

	if(varType=="platemodel"){
		# check for non-related input
		if(res!=1) warning("Argument 'res' is ignored for plate model fetching.")
		combined <- fetchModel(dat=dat, var=var, ver=ver, datadir=datadir, register=register, verbose=verbose)
	}

	# display citations
	if(verbose){
		message("If you use the data in publications, please cite its\nreference, as well as that of the 'chronosphere' package.")
		for(i in 1:length(combined$citation)){
			cat("\n")
			message(paste("-", combined$citation[i]))
		}
	}
	
	return(combined$final)
	
}

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
		ver <- varReg[order(varReg[,"ver"], decreasing=TRUE)==1,"ver"]
	
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


# Raster-specific submodule of fetch()
fetchRaster <- function(dat, var, res=1, ver=NULL, datadir=NULL, register=register, verbose=TRUE){
	if(! requireNamespace("ncdf4", quietly=TRUE)) stop("This method requires the 'ncdf4' package to run.")
	
	# the data have to use the same resolution!!!
	if(length(res)>1) stop("Only one resolution can be used in a single download call.")

	# subset the register to the resolution of interest
	register <- register[register[, "res"]==res, , drop=FALSE] 

	# check whether resolution is there, before the download
	for(j in 1:length(var)){
		if(sum(register[,"var"]==var[j])==0){
			stop(paste("The variable '", var[j], "' does not exist at the desired resolution (", res, "). ", sep=""))
		}
	}

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
			version <- varReg[order(varReg[,"ver"], decreasing=TRUE)==1,"ver"]
		
		# version number is given 
		}else{
			if(!any(version==varReg[, "ver"])) stop(paste("Invalid variable version for ", var[j], sep=""))
		}
		
		# Check whether download is required or not
		# the name of the res_variable_ver-specific archive
		archive<- paste(resChar,"_",  var[j],"_", version, ".zip", sep="")

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
					download.file(paste(remote, dat,"/",  var[j], "/", archive,  sep = ""),pathToArchive, mode="wb", quiet=!verbose)
				}else{
					download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  archive,  sep = ""),pathToArchive, mode="wb", quiet=!verbose)
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
				download.file(paste(remote, dat,"/",  var[j], "/", archive,  sep = ""),temp, mode="wb", quiet=!verbose)
			}else{
				download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  var[j], "/", archive,  sep = ""),temp, mode="wb", quiet=!verbose)
			}
			
		
			# unzip it in temporary directory
			unzip(temp, exdir=tempd)
		
			# get rid of the archive
			unlink(temp)
		}

		
		# read all files in temporary directory
		all<-list.files(tempd)
	
			# resolution names have to be here, otherwise they will be overwritten here and the links to the 
			# hard drive versions will break

			# select the ones that were just downloaded, current variable
			all<- all[unlist(
				lapply(strsplit(all, "_"), function(x){
				 	paste(x[1:3], collapse="_")
				})
				)==paste(resChar, var[j],version, sep="_")]
		
			# order them
			# # future layer names
			#splittedExt <- strsplit(all, "[^.]+$")
			
			# resolution_variable_age
			#resVarAge <- unlist(lapply(splittedExt, function(x)x[1]))
			resVarVerAge <- sub("\\.[[:alnum:]]+$", "", all)
			
			# remove the version too
			resVarAge <- sub(paste("_",version, sep=""), "", resVarVerAge)

			# variable_age - future layer name
			# varAge <- unlist(
			#   lapply(strsplit(resVarAge, "_"), function(x){
			#     paste(x[2:3], collapse="_")
			#   })
			# )
			# 
			# remove resolution from the variable name
			varAge <- gsub('([A-za-z0-9]+)_([A-Za-z]+_[0-9.]+$)', '\\2', resVarAge)

			# get the extension of the file for later decisions 
			# extension <- unlist(lapply(splittedExt, function(x)x[[2]]))[1]
			extension <- regmatches(all, gregexpr("[^.]+$", all))[[1]]

			# only the ages
			# ageNum <- as.numeric(unlist(lapply(strsplit(varAge, "_"), function(x) x[[2]])))

			# get ages 
		 	 ageNum <- as.numeric(unlist(regmatches(varAge, gregexpr("[0-9]\\d{0,9}(\\.\\d{1,3})?%?$", varAge))))

			# ordered file names
			all<-all[order(ageNum)]
	
		
		# to make a stack in memory, make a list first
		listForm <- list()
		
		# netCDF files (raster data - two dimension!)
		if(extension=="nc"){
			for(i in 1:length(all)){
				listForm[[i]]<-raster::raster(file.path(tempd, all[i]))
				listForm[[i]]@data@names <- varAge[order(ageNum)][i]
			}
			# when the variable is a raster
			# make a RasterArray
			ind <- 1:length(all)
			names(ind) <-  as.character(sort(ageNum))
		}

		# JPEG (images)
		if(extension=="jpg"){
			# check for the presence of the jpeg package
			
			# iterate, this will be the stack, later
			for(i in 1:length(all)){
				# import all three channells
				scot <- raster::stack(paste0(tempd, "/", all[i]))
				
				#redefine extent 
				extent(scot) <- extent(-180, 180, -90, 90)
				
				#name them properly
				names(scot) <- paste(varAge[order(ageNum)][i], c("R", "G", "B"), sep="_")
				
				# add them to list
				listForm[[i]]<- scot[[1]]
				listForm[[i+length(all)]]<- scot[[2]]
				listForm[[i+length(all)*2]]<- scot[[3]]

				# add the projections
				listForm[[i]]@crs@projargs<-"+proj=longlat"
				listForm[[i+length(all)]]@crs@projargs<-"+proj=longlat"
				listForm[[i+length(all)*2]]@crs@projargs<-"+proj=longlat"
			}
		  
			# the proxy object structure of the RasterArray
			ind <- matrix(1:length(listForm), ncol=3, byrow=FALSE)
			colnames(ind) <- c("paleoatlas_R", "paleoatlas_G", "paleoatlas_B")
			rownames(ind) <-  as.character(sort(ageNum))
		}
		
		# RasterArray
		varObj[[j]] <- RasterArray(stack=raster::stack(listForm), index=ind)
	
		# 'get rid of' temporary directory
		unlink(tempd)
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

