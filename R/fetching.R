# remote server 
remote <- "https://www.cnidaria.nat.fau.de/tersane/public/public/"
userpwd <- NULL

#' Download a database extract from \code{chronosphere} remote server
#' 
#' The function will download a list of available data from the data repository
#' 
#' The function will download a single .csv file and attached it as a \code{data.frame}.
#' 
#' @param datadir Directory where the downloaded file 'reg.csv' file is kept. Individual layers will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#'
#' @export
dataindex <- function(datadir=NULL){
		
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

		# you can set target file, won't change anything if there is nothing to download
		temp<- file.path(datadir, "reg.csv")

	# need to download but not saved
	}else{
		# temporary file
		temp <- tempfile()
	}

	# go on with download
	if(download){
		# do the download
		if(is.null(userpwd)){
			download.file(paste(remote, "reg.csv", sep = ""),temp, mode="wb")
		}else{
			download.file(paste("ftp://", userpwd, "@",remote, "reg.csv", sep = ""),temp, mode="wb")
		}	
		
		# and set return value
		ret <- read.csv(temp, sep=",", header=TRUE, stringsAsFactors=FALSE)

		# get rid of the  temporary file
		if(is.null(datadir)) unlink(temp)
	
	}

	return(ret)
}

#' Data layer fetching
#' 
#' Function to download and attach variables in the \code{chronosphere} package
#' 
#' The function is implemented for a single variable, which can be downloaded and attached efficiently (dem). There are a lot of things to figure out.
#' Version 2.
#' @param dat The dataset to get variables from.
#' @param var Vector of variable names to get.
#' @param res The resolution of the layers. This has to be the same for all layers.
#' @param ver The version of the variable. Defaults to \code{NULL}, which will download the latest available version. We have to create a data table, which should be part of the package. This has to be searched for valid argument combinations. Right this is just a folder with a date.
#' @param datadir Directory where downloaded files are kept. Individual layers will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#' @examples
#' 	a <- fetch(dat="paleomap", var="dem")
#' 	b <- fetch(dat="paleomap", var="paleoatlas", res=0.1)
#' @export
fetch <- function(dat, var, res=1, ver=NULL, datadir=NULL){
	# get the remote server data, or read it from hard drive!
	register <- dataindex(datadir=datadir)

	# the data have to use the same resolution!!!
	if(length(res)>1) stop("Only one resolution can be used in a single download call.")

	# and select that part
	register <- register[register[, "res"]==res, , drop=FALSE]

	# if one of the variables do not exist, then omit it
	present <- var%in%register$var

	# stop if not present
	if(any(!present)){
		if(length(present)==1) stop("Variable does not exist at the given resolution.")
		if(sum(present)==0) stop("No given variables exist at the given resolution.")
		
		# some do, but some are missing
		warning(paste("The variable(s) '",paste(var[!present],collapse="', '"), "' do not exist at \nthe given resolution and are omitted.",  sep=""))
		var<-var[present]
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
	varObj <-list()
	# for all the variables
	for(j in 1:length(var)){
		# current version - NA to use default
		version <- verLong[j]

		# check structure database, whether the given verison is alright
		varReg<- register[register[,"var"]==var[j],, drop=FALSE]

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
				download.file(paste(remote, dat,"/",  res,"/", var[j], "/", archive,  sep = ""),pathToArchive, mode="wb")
			}

			# and unzip to a temporary directory
			unzip(pathToArchive, exdir=tempd)

		# must download
		}else{
			# temporary files
			temp <- tempfile()
		
			# download archive
			if(is.null(userpwd)){
				download.file(paste(remote, dat,"/",  res,"/", var[j], "/", archive,  sep = ""),temp, mode="wb")
			}else{
				download.file(paste("ftp://", userpwd, "@",remote, dat,"/",  res,"/", var[j], "/", res,"_",  var[j],"_", version, ".zip",  sep = ""),temp, mode="wb")
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
			final<- cbind2(final, nex)
		}
		colnames(final) <- var
	}

	return(final)
	
}

