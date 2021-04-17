###########################################################################
# Front-end wrapper function

#' Reconstruct geographic features
#' 
#' Reconstruct the geographic locations from present day coordinates and spatial objects back to their paleo-positions. 
#' Each location will be assigned a plate id and moved back in time using the chosen reconstruction model.
#' 
#' The function implements two reconstruction submodules, which are selected with the \code{model} argument:
#' 
#' If \code{model} is a \code{character} entry, then the \code{reconstruct()} function uses the GPlates Web Service (\url{https://gws.gplates.org/}, remote reconstruction submodule).
#' The available reconstruction models for this submodule are:
#' \itemize{
#'	 \item "SETON2012" (Seton et al., 2012) for coastlines and plate polygons.
#'	 \item "MULLER2016" (Muller et al., 2016) for coastlines and plate polygons.
#'	 \item "GOLONKA" (Wright et al. 2013) for coastlines only. 
#'	 \item "PALEOMAP" (Scotese and Wright, 2018) for coastlines and plate polygons. 
#'	 \item "MATTHEWS2016" (Matthews et al., 2016) for coastlines and plate polygons. 
#' }
#' 
#' If \code{model} is a \code{\link{platemodel}} class object, then the function will try to use the GPLates desktop application (\url{https://www.gplates.org/}) to reconstruct the coordinates (local reconstruction submodule).
#' Plate models are available in chronosphere with the \code{\link{fetch}} function. See \code{\link{datasets}} for the available models.
#' The function will try to find the main GPlates executable in its default installation directory. If this does not succeed, use \code{path.gplates} to enter the full path to the GPlates executable as a \code{character} string.
#' 
#' 
#' @section References:
#' Matthews, K. J., Maloney, K. T., Zahirovic, S., Williams, S. E., Seton, M., & Müller, R. D. (2016). Global plate boundary evolution and kinematics since the late Paleozoic. Global and Planetary Change, 146, 226–250. https://doi.org/10.1016/j.gloplacha.2016.10.002
#' \cr
#' \cr Müller, R. D., Seton, M., Zahirovic, S., Williams, S. E., Matthews, K. J., Wright, N. M., … Cannon, J. (2016). Ocean Basin Evolution and Global-Scale Plate Reorganization Events Since Pangea Breakup. Annual Review of Earth and Planetary Sciences, 44(1), 107–138. https://doi.org/10.1146/annurev-earth-060115-012211
#' \cr
#' \cr Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models (PaleoDEMS) for the Phanerozoic PALEOMAP Project. Retrieved from https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/
#' \cr
#' \cr Seton, M., Müller, R. D., Zahirovic, S., Gaina, C., Torsvik, T., Shephard, G., … Chandler, M. (2012). Global continental and ocean basin reconstructions since 200Ma. Earth-Science Reviews, 113(3–4), 212–270. https://doi.org/10.1016/j.earscirev.2012.03.002
#' \cr
#' \cr Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards community-driven paleogeographic reconstructions: integrating open-access paleogeographic and paleobiology data with plate tectonics. Biogeosciences, 10(3), 1529–1541. https://doi.org/10.5194/bg-10-1529-2013
#' 
#' @param x are the features to be reconstructed. Can be a vector with longitude and latitude representing
#' a single point or a matrix/dataframe with the first column as longitude and second column as latitude, or a \code{SpatialPolygonsDataFrame} class object. 
#' The character strings \code{"plates"} and \code{"coastlines"} return static plates and rotated present-day coastlines, respectively.
#' @param ... arguments passed to class-specific methods.
#' @param age (\code{numeric})is the age in Ma at which the points will be reconstructed
#' @param model (\code{character} or \code{\link{platemodel}}) The  reconstruction model. The class of this argument selects the submodule used for reconstruction, a \code{character} value will invoke the remote reconstruction submodule and will submit \code{x} to the GPlates Web Service. A \code{platemodel} class object will call the local-reconstruction submodule. The default is \code{"PALEOMAP"}. See details for available models.
#' @param reverse (\code{logical}) Argument of the remote reconstruction submodule. The flag to control the direction of reconstruction. If \code{reverse = TRUE}, the function will 
#' calculate the present-day coordinates of the given paleo-coordinates. 
#' @param path.gplates (\code{character}) Argument of the local reconstruction submodule. In case the GPlates executable file is not found at the coded default location, the full path to the executable (gplates-<ver>.exe on Windows) can be entered here.
#' @param listout (\code{logical})If multiple ages are given, the output can be returned as a \code{list} if \code{listout = TRUE}.
#' @param verbose (\code{logical}) Should call URLs (remote submodule) or console feedback (local-submodule) be printed?
#' @param cleanup (\code{logical}) Argument of the local reconstruction submodule. Should the temporary files be deleted immediately after reconstructions?
#' @param plateperiod (\code{logical}) Argument of the local reconstruction submodule. Should the durations of the plates be forced on the partitioned feature? If these are set to \code{TRUE} and the plate duration estimates are long, then you might lose some data.
#' @param dir (\code{character}) Argument of the local reconstruction submodule. Directory where the temporary files of the reconstruction are stored (defaults to a temporary directory created by R). Remember to toggle \code{cleanup} if you want to see the files.  
#' @return A \code{numeric} matrix if \code{x} is a \code{numeric}, \code{matrix} or \code{data.frame}, or \code{Spatial*} class objects, depending on input.
#' @examples
#' # With the web service (GPlates Web Service was offline at submission)
#' # simple matrices
#' # reconstruct(matrix(c(95, 54), nrow=1), 140)
#'	
#'	# points reconstruction
#'	xy <-cbind(long=c(95,142), lat=c(54, -33))
#'	# reconstruct(xy, 140)
#'
#' # coastlines/plates
#' # coast <- reconstruct("coastlines", 140)
#' # plate <- reconstruct("plates", 139)
#'	
#' @rdname reconstruct
#' @exportMethod reconstruct
setGeneric("reconstruct", function(x,...) standardGeneric("reconstruct"))

# have to use long function definitions for documentation.
#' @param enumerate (\code{logical}) Should be all coordinate/age combinations be enumerated and reconstructed (set to \code{TRUE} by default)? \code{FALSE} is applicable only if the number of rows in \code{x} is equal to the number elementes in \code{age}. Then a point will be reconstructed to the age that has the same index in \code{age} as the row of the coordinates in \code{x}. List output is not available in this case. 
#' @param chunk (\code{numeric}) Argument of the remote reconstruction submodule. Single integer, the number of coordinates that will be queried from the GPlates in a single go. 
#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="matrix", 
	function(x,age, model="PALEOMAP", listout=TRUE, verbose=FALSE, enumerate=TRUE, chunk=200, reverse=FALSE, path.gplates=NULL, cleanup=TRUE, dir=NULL,plateperiod=FALSE){
	 
		# Check long lat!
		if(!is.numeric(age)) age <- as.numeric(age)

		# depending on length
		if(length(age)>1){
		 
			# base condition of enumerate=FALSE
				if(!enumerate & length(age)!=nrow(x)){
						enumerate <- TRUE
						warning("Enumerating coordinate-age combinations. \n enumerate = FALSE is possible only if the number of coordinates matches the number of ages.")
			} 

			# if the function is allowed to enumerate
			if(enumerate){
				# depending on output
				if(listout){
					container<- list()
			 
				# 3d matrix
				}else{
					container <- array(NA, dim=c(length(age), dim(x)))
				}
	
				# iterate over ages
				for(i in 1:length(age)){
                    if(is.character(model)){
					   fresh <- IteratedPointReconstruction(coords=x, chunk=chunk, age=age[i], model=model, reverse=reverse, verbose=verbose)
                    }else{
                        fresh <- reconstructGPlates(x=x, age=age[i], model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
                    }
					# list
					if(listout){
						container[[i]] <- fresh
					# 3d matrix
					}else{
						container[i,,] <- fresh
					}
				}
	
				# name the	output
				# list
				if(listout){
					names(container) <- age
				# matrix
				}else{
					dimnames(container) <- c(list(age), dimnames(fresh))
				}

			# used vectorized age implementation, no enumeration
			}else{
				# empty container
				container <- x
				container[]<-NA

				# reconstruction is performance-capped, for loop should be enough
				ageLevs <- unique(age)

				# for all different age values
				for(i in 1:length(ageLevs)){
					# which rows apply
					index <- which(ageLevs[i]==age)
					current <- x[index, , drop=FALSE]
					# do reconstruction and store
                    if(is.character(model)){
					   container[index,] <- IteratedPointReconstruction(coords=current, chunk=chunk, age=ageLevs[i], model=model, reverse=reverse, verbose=verbose)
				    }else{
                        container[index,] <- reconstructGPlates(x=current, age=ageLevs[i], model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
                    }
                }
			}

		# single target
		}else{
            if(is.character(model)){
                container <- IteratedPointReconstruction(coords=x, chunk=chunk, age=age, model=model, reverse=reverse, verbose=verbose)
            }else{
                container <- reconstructGPlates(x=x, age=age, model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
            }
		}

		# and return
		return(container)
	} 
)


#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="data.frame", 
	function(x,... ){
		reconstruct(as.matrix(x), ...)
})

#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="numeric", 
	function(x,... ){
		if(length(x)==2) reconstruct(matrix(x, nrow=1), ...) else stop("Only 2 element vectors are allowed!")
})

#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="character", 
	function(x,age, model="PALEOMAP", listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE){
        if(!any(x==c("plates", "coastlines"))) stop("Invalid 'x' argument.\nThe only valid character input are \"plates\" and \"coastlines\"")
		# vectorized
		if(length(age)>1){
			
			# list
			if(listout){
				container<- list()
			# SpArray to be
			}else{
				stop("Noooo, not yet!")
			}
			# iterate over ages
			for(i in 1:length(age)){
				# what is needed?
				if(x=="coastlines"){
					feature <- gplates_reconstruct_coastlines(age=age[i], model=model, verbose=verbose)
				}

				if(x=="plates"){
					if(is.character(model)){
						feature <- gplates_reconstruct_static_polygons(age=age[i], model=model, verbose=verbose)
					}else{
						feature <- reconstructGPlates(x="plates", age=age[i], model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
					}
				}

				# save it
				container[[i]] <- feature
			}
			# list output
			if(listout){
			 names(container) <- age
			}

		# single entry
		}else{
			# what do you want?
			if(x=="coastlines"){
				container <- gplates_reconstruct_coastlines(age=age, model=model, verbose=verbose)
			}

			if(x=="plates"){
				if(is.character(model)){
					container <- gplates_reconstruct_static_polygons(age=age, model=model, verbose=verbose)
				}else{
					container <- reconstructGPlates(x="plates", age=age, model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
				}
			}
		}
		# return container
		return(container)
	} 
)


#' @rdname reconstruct
setMethod(
	"reconstruct",
	"SpatialPolygonsDataFrame", 
	function(x, age, model="PALEOMAP", listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE){
		if(!is.na(x@proj4string)){
			x <- sp::spTransform(x, sp::CRS("+proj=longlat"))
		}else{
			x@proj4string <- sp::CRS("+proj=longlat")
		}

		# vectorized implementation
		if(length(age)>1){
			# list output
			if(listout){
				container <- list()

			# SpArray
			}else{
				stop("Nooo, not yet!")	
			}

			# iterate
			for(i in 1:length(age)){
				if(is.character(model)){
					container[[i]] <- gplates_reconstruct_polygon(sp=x, age=age[i], model=model, verbose=verbose)
				}else{
					container[[i]] <- reconstructGPlates(x=x, age=age[i], model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup)
				}
				# rotate entry back
				container[[i]] <- sp::spTransform(container[[i]], x@proj4string)
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(is.character(model)){
				container <- gplates_reconstruct_polygon(sp=x, age, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup)
			}
			container <- sp::spTransform(container, x@proj4string)
			
		}

		return(container)

	}
)

#' @rdname reconstruct
setMethod(
	"reconstruct",
	"SpatialLinesDataFrame", 
	function(x, age, model="PALEOMAP", listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE){
		if(!is.na(x@proj4string)){
			x <- sp::spTransform(x, sp::CRS("+proj=longlat"))
		}else{
			x@proj4string <- sp::CRS("+proj=longlat")
		}

		# vectorized implementation
		if(length(age)>1){
			# list output
			if(listout){
				container <- list()

			# SpArray
			}else{
				stop("Nooo, not yet!")	
			}

			# iterate
			for(i in 1:length(age)){
				if(is.character(model)){
					# might not work!!
					container[[i]] <- gplates_reconstruct_polygon(sp=x, age=age[i], model=model, verbose=verbose)
				}else{
					container[[i]] <- reconstructGPlates(x=x, age=age[i], model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup)
				}
				# rotate entry back
				container[[i]] <- sp::spTransform(container[[i]], x@proj4string)
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(is.character(model)){
				container <- gplates_reconstruct_polygon(sp=x, age, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup)
			}
			container <- sp::spTransform(container, x@proj4string)
			
		}

		return(container)

	}
)

###########################################################################
# Offline interface to Gplates


#Mac examples
#	library(chronosphere)
#	x<- fetch("pared", "public")[,c("longitude", "latitude")]
#	mo <- fetch("paleomap", "model")
#	reconstruct(x, age=10, model=mo, verbose=TRUE)
#reconstruct(x, age=10, model=mo, verbose=TRUE, path.gplates="/Users/Nick/Downloads/GPlates-2.2.0/gplates.app/Contents/MacOS/gplates")



reconstructGPlates <- function(x, age, model, path.gplates=NULL,dir=NULL, verbose=FALSE, cleanup=TRUE, plateperiod=FALSE){
	if(class(model)!="platemodel") stop("You need a GPlates tectonic model for this method.")
	if(! requireNamespace("rgdal", quietly=TRUE)) stop("This method requires the 'rgdal' package to run")
	
    # 1. FIND GPlates
		# A. get operating system
		os <- getOS()

		# B. should the program look for a default path for gplates?
		if(is.null(path.gplates)){
			# depending on the os
			if(os=="linux"){
				# the GPlates executable itself
				gplatesExecutable <- "gplates"

				# what the user would have entered
				path.gplates <- gplatesExecutable

				# leave the model intact in the namespace (easier debug)
				rotation <- model@rotation
				platePolygons <- model@polygons

				# separator character between directories
				dirSep <- "/"

			}
			if(os=="windows"){
				# 1. find GPLATES exectutable if possible
				# directory and executable
				gplatesPaths <- winDefaultGPlates()
				#path to executable
				path.gplates <- paste(gplatesPaths, collapse="/")
				# system call to executable
				gplatesExecutable <- paste("\"", path.gplates, "\"", sep="")

				# 2. replace model paths with \\
				rotation <- gsub("/","\\\\", model@rotation)
				platePolygons <- gsub("/","\\\\", model@polygons)

				# characters to include directory 
				dirSep <- "\\\\"

			}
			if(os=="osx"){
				# the GPlates executable itself
				# default
				# gplatesExecutable <- "/Applications/GPlates-2.2.0/gplates.app/Contents/MacOS/gplates"
				gplatesPaths <- macDefaultGPlates()
				gplatesExecutable <- paste(gplatesPaths, collapse="/")
				
				# what the user would have entered
				path.gplates <- gplatesExecutable

				# leave the model intact in the namespace (easier debug)
				rotation <- model@rotation
				platePolygons <- model@polygons

				# separator character between directories
				dirSep <- "/"
				
			}

		# look for given path
		}else{
			# separate to form a length 2 vector
			gplatesExecutable <- path.gplates
			
			# leave the model intact in the namespace (easier debug)
			rotation <- model@rotation
			platePolygons <- model@polygons

			# separator character between directories
			dirSep <- "/"
			
			# windows needs special treatment
			if(os=="windows"){
			
				# system call to executable
				gplatesExecutable <- paste("\"", path.gplates, "\"", sep="")

				# 2. replace model paths with \\
				rotation <- gsub("/","\\\\", model@rotation)
				platePolygons <- gsub("/","\\\\", model@polygons)

				# characters to include directory 
				dirSep <- "\\\\"

			}
			
		}

		# C. one moretest whether gplates was detected or not
			gpTest <- testGPlates(gplatesExecutable, verbose=verbose)

			# if gplates is not present:
			if(!gpTest) stop(paste("The GPlates executable\n	\"", path.gplates,"\"\nwas not found.", sep=""))

	# 2. Setup reconstruction environment
		# folder where files will be executed
		if(is.null(dir)) tempd <- tempdir() else tempd <- dir

		# prepare x
		# create a SpatialPointsDataFrame from long-lat matrix
		if("matrix"%in%class(x) | "data.frame"%in%class(x)){
			spPoints<- sp::SpatialPoints(x)
			spPoints@proj4string <- sp::CRS("+proj=longlat")
			xTransform <- sp::SpatialPointsDataFrame(spPoints, data=data.frame(a=1:nrow(x)))		
		}
		
		# if originally a SpatialPointsDataFrame
		if("SpatialPointsDataFrame"%in%class(x)){
			if(!is.na(x@proj4string)){
				xTransform <- sp::spTransform(x, sp::CRS("+proj=longlat"))
			}else{
				x@proj4string <- sp::CRS("+proj=longlat")
				xTransform <- x
			}
		}

		# if originally a SpatialPointsDataFrame
		if("SpatialPolygonsDataFrame"%in%class(x) | "SpatialLinesDataFrame"%in%class(x)){
			xTransform <- x
		}

		# in case stat
		if(!is.character(x)){
		# write 'x' as a shapefile
			layer<- paste(randomString(length=3), age, sep="_")
			if(verbose) message(paste("Exported data identified as ", layer))
			pathToFileNoEXT <- paste(tempd, "/", layer,sep="")
			if(verbose) message("Exporting 'x' as a shapefile.")
			rgdal::writeOGR(xTransform, dsn=paste(pathToFileNoEXT, ".shp", sep=""), layer=layer, driver="ESRI Shapefile")
		}else{
		# feature to reconstruct is the static polygons
			if(length(x)!=1) stop("Only the 'plates' can be reconstructed with this method.")
			if(x=="plates"){
				# use original one - even for windows.
				pathToFileNoEXT <- gsub(".gpml", "",model@polygons)
			}
		}

		# inheritance of appearance and disappearance dates
		if(plateperiod){
			pPer <- 1
		}else{
			pPer <- 0
		}
		
	# 3. Execute GPlates commands
		# convert to gpml
		if(!is.character(x)){
			if(verbose) message("Converting shapefile to .gpml.")
			conversion <- paste(gplatesExecutable, " convert-file-format -l ",pathToFileNoEXT,".shp -e gpml", sep="")
			system(conversion, ignore.stdout=!verbose,ignore.stderr=!verbose)
		}
		# do the plate assignment
		if(!is.character(x)){
			if(verbose) message("Assigning plate IDs to .gpml file.")
			assignment <- paste(gplatesExecutable, " assign-plate-ids -e ",pPer," -p ", platePolygons, " -l ",pathToFileNoEXT,".gpml", sep="")
			system(assignment, ignore.stdout=!verbose,ignore.stderr=!verbose)
		}
		# do reconstruction
		if(!is.character(x)) if(verbose) message("Reconstructing coordinates.")
		if(is.character(x)) if(x=="plates") if(verbose) message("Reconstructing plates.")
			reconstruction <- paste(gplatesExecutable, " reconstruct -l ",pathToFileNoEXT,".gpml -r ", 
					rotation, " -e shapefile -t ", age, " -o ", pathToFileNoEXT,"_reconstructed -w 1", sep="") 
			system(reconstruction, ignore.stdout=!verbose,ignore.stderr=!verbose)

	# 4. Processing output
		# reading coordinates
		if(!is.character(x)){
			if(verbose) message("Reading reconstructed coordinates.")
			somethingDF <- rgdal::readOGR(paste(pathToFileNoEXT,"_reconstructed.shx",	sep=""), verbose=verbose)
		} 
		if(is.character(x)){
			if(x=="plates") if(verbose) message("Reading plates.")
			pathToFile <- paste(pathToFileNoEXT,"_reconstructed",dirSep ,fileFromPath(pathToFileNoEXT),"_reconstructed_polygon.shx", sep="")
			somethingDF <- rgdal::readOGR(pathToFile, verbose=verbose)
		}
		
		# transform object back to whatever it was
		if("matrix"%in%class(x) | "data.frame"%in%class(x)){
			# some coordinates probably were missing
			rotated <- x
			rotated <- matrix(NA, ncol=2, nrow=nrow(x))
			colnames(rotated) <- colnames(x)
			rownames(rotated) <- rownames(x)
			rotated[somethingDF@data$a,]  <- somethingDF@coords
			
		}

		if(
			"SpatialPolygonsDataFrame"%in%class(x) | 
			"SpatialPointsDataFrame"%in%class(x) |
			"SpatialLinesDataFrame"%in%class(x) |
			"character"%in%class(x)){
				rotated <- somethingDF
		}


	# 5. Finish
		# remove temporary files
		if(!"character"%in%class(x)){
			if(cleanup){		
				system(paste("rm ",tempd, "/",layer,"*", sep=""))
			}
		}
	return(rotated)
}

# function to find Gplates installation directory
winDefaultGPlates<-function(){

	# default installation paths
	basic <- c(
		"C:/Program Files/GPlates",
		"C:/Program Files (x86)/GPlates"
		
	)

	versioned <- NULL
	inWhich <- NULL

	# search both possible folders 
	for(i in 1:2){
		# enter program files
		gpver <- list.files(basic[i])
	
		found <- grep("GPlates", gpver)
	
		# grab the latest version
		if(length(found)>0){
			versioned <- gpver[found[length(found)]]
			inWhich <- i
		}
	}
	if(is.null(inWhich)) stop("Could not locate GPlates.")

	# add it 
	dir <- file.path(basic[inWhich], versioned)

	# search executable
	gpfiles <- list.files(dir)

	# grab gplates executable file
	gplat <- grep("gplat",gpfiles)
	potExe <- gpfiles[gplat]
	exe <- potExe[grep("exe",potExe)]

	return(c(dir=dir, exe=exe))
}


macDefaultGPlates <-function(){
# default installation path
	basic <- "/Applications"
	# enter program files
	gpver <- list.files(basic)

	found <- grep("GPlates", gpver)

	# grab the latest version
	if(length(found)>0){
		gpver<- gpver[found[length(found)]]
	}else{
		stop("Could not locate GPlates.")
	}
	dir <-file.path(basic,gpver, "gplates.app/Contents/MacOS")
	exe <-"gplates"
	return(c(dir=dir, exe=exe))
}



testGPlates<- function(gplatesExecutable, verbose){
# ask version
	gplatesTest <- paste(gplatesExecutable, "--v")
	
	# "\"C:/Program Files (x86)/GPlates/GPlates 2.1.0/gplates-2.1.0.exe\" --v"
	# default version
	ver <- NULL
	
	# depending on how much the user wants to see
	if(!verbose){
		opt <- options(show.error.messages = FALSE)
		# revert even if command below fails for some reason
		on.exit(options(opt))

		try(ver <- system(gplatesTest, intern=TRUE,ignore.stdout = TRUE, 
				ignore.stderr = TRUE))
	}else{
		try(ver <- system(gplatesTest, intern=TRUE))
	}
	
	# if gplates is not present
	return(!is.null(ver))
}


###########################################################################
# GPlates Web Service internals:

# correcting the point recontstruction problem, wrapper around the point reconstruction funciton
IteratedPointReconstruction <- function(coords,age, chunk=200, model="PALEOMAP", reverse=FALSE, verbose=TRUE){
	# number of coordinates
	coordNum <- nrow(coords)
	
	# do only when the number of coordinates is large enough
	if(coordNum>chunk){
		# batch number
		moreIndex <- rep(1:ceiling(coordNum/chunk), each=chunk)
		index<-moreIndex[1:coordNum]
		
		# new container
		newCoords <- matrix(NA, ncol=2, nrow=coordNum)
		colnames(newCoords) <- colnames(coords)
		rownames(newCoords) <- rownames(coords)

		# the number of batches 
		maxIndex <- max(index)
	 
		# iterate - for() is easier, performance impact unlikely
		for(i in 1:maxIndex){
			# index of batch number
			bIndex <- index==i
	
			# current batch 
			current <- coords[bIndex,]

			# do the reconstruction
			tryCatch({
					iterRes <- gplates_reconstruct_points(current, age=age, model=model, reverse=reverse, verbose=verbose)
				},
				error=function(cond){
					stop("Query URL is too long. Round coordinates or decrease chunk size.")
				}
			) 

			
			# store
			newCoords[bIndex,] <- iterRes
		}

	# save some time by skipping this
	}else{
		tryCatch({
			newCoords <- gplates_reconstruct_points(coords, age=age, model=model, reverse=reverse, verbose=verbose)
		}, error=function(cond){
					stop("Query URL is too long. Round coordinates or decrease chunk size.")
			 }
		)

	}

	return(newCoords)

}



# Reconstruct points
# 
# Reconstruct the geographic locations from present day coordinates back to their paleo-positions. 
# Each location will be assigned a plate id and moved back in time using the chosen reconstruction model.
# 
# Adapted from GPlates Web Service (need to find out how to reference them)
# 
# @param coords are the coordinates to be reconstructed. Can be a vector with longitude and latitude representing
# a single point or a matrix/dataframe with the first column as longitude and second column as latitude
# @param age is the age in Ma at which the points will be reconstructed
# @param	model is the reconstruction model. The default is "PALEOMAP". Add more details about additional models here
# @param reverse the flag to control the direction of reconstruction. If reverse = TRUE, the function will 
# calculate the present-day coordinates of the given paleo-coordinates.
# @param verbose Should the function output urls?
#
# @return matrix with longitude and latitude	
# 
# @examples
# gplates_reconstruct_points(c(95, 54), 140)
# 
# xy <-cbind(long=c(95,142), lat=c(54, -33))
# gplates_reconstruct_points(xy, 140)
gplates_reconstruct_points <- function(coords,age, model="PALEOMAP", reverse=FALSE, verbose=TRUE){
	
	url <- 'https://gws.gplates.org/reconstruct/reconstruct_points/'
	
	#multiple points, as matrix or dataframe
	if(is.matrix(coords) | is.data.frame(coords)){
		coords <- toString(as.vector(t(coords)))
	}
	
	#single points as vector
	if(is.vector(coords)){ 
		coords <- toString(coords)
	}
	
	#spatial points or spatial points data frame
	if ((attr(regexpr("SpatialPoints", class(coords)),"match.length") > 0)){
		coords <- toString(t(coordinates(coords)))
	}
	
	#fetch data
	query <- sprintf('?points=%s&time=%d&model=%s',gsub(" ", "", coords),age, model)
	
	# for reconstruction of present day coordinates from paleocoordinates
	if (reverse == TRUE){
		query <- paste0(query, "&reverse")
		cols <- c("long", "lat")
	} else cols <- c("paleolong", "paleolat")
	
	fullrequest <- sprintf(paste0(url,query, "&return_null_points"))
	
	if(verbose) cat("Extracting coordinates from:", fullrequest, "\n")
	rawdata <- readLines(fullrequest, warn="F") 
	
	#if null
	rawdata <- gsub("null", "[[-9999, -9999]]", rawdata)
	
	#extract coordinates
	rcoords <- matrix(as.numeric(unlist(regmatches(rawdata, gregexpr("-?[[:digit:]]+\\.*[[:digit:]]+", rawdata)))), ncol=2, byrow = TRUE)
	rcoords[rcoords == -9999] <- NA #replace na values
	
	colnames(rcoords) <- cols
	return(rcoords)
}

# Reconstruct coastlines
# Retrieve reconstructed coastline polygons for defined ages
# 
# @param age is the age in Ma at which the points will be reconstructed
# @param model is the reconstruction model. The default is "PALEOMAP". Add more details about additional models here
# @param verbose Should the function output urls?
# @return SpatialPolygonsDataFrame
gplates_reconstruct_coastlines <- function(age, model="PALEOMAP", verbose=TRUE){
	
	#download and save data
	url <- 'http://gws.gplates.org/reconstruct/coastlines/'
	query <- sprintf('?time=%d&model=%s', age, model)
	
	fullrequest <- sprintf(paste0(url,query))
	if(verbose) cat("Getting data from:", fullrequest, "\n")

	r <- readLines(fullrequest, warn=FALSE)
	
	#read data
	dat <- rgdal::readOGR(r, "OGRGeoJSON", verbose = FALSE)
	
	return(dat)
}

#  reconstruct static polygons
#  
#  Retrieve reconstructed static polygons for defined ages
#  
#  @param age is the age in Ma at which the points will be reconstructed
#  @param	model is the reconstruction model. The default is "PALEOMAP". Add more details about additional models here
#  @param verbose Should the function output urls?
#  
# @return SpatialPolygonsDataFrame
# 
# @examples
#  gplates_reconstruct_static_polygons(140)
#  
gplates_reconstruct_static_polygons <- function(age, model="PALEOMAP", verbose=TRUE){
	if(! requireNamespace("rgdal", quietly=TRUE)) stop("This method requires the 'rgdal' package to run.")
	
	#download and save data
	url <- 'http://gws.gplates.org/reconstruct/static_polygons/'
	query <- sprintf('?time=%d&model=%s',age, model)
	
	fullrequest <- sprintf(paste0(url,query))
	if(verbose) cat("Getting data from:", fullrequest, "\n")
	
	r <- readLines(fullrequest, warn=FALSE)
	
	#read data
	dat <- rgdal::readOGR(r, "OGRGeoJSON", verbose = FALSE)
	
	return(dat)
}

#  reconstructing polygons
#  
#  @param sp is a SpatialPolygonsDataFrame
#  @param verbose Should the function output urls?
#  
gplates_reconstruct_polygon <- function(sp, age, model="PALEOMAP", verbose=TRUE){
	
	url = 'https://gws.gplates.org/reconstruct/reconstruct_feature_collection/'
	
	#extract coordinates
	polys = attr(sp,'polygons')
	npolys = length(polys)
	for (i in 1:npolys){
		poly = polys[[i]]
		polys2 = attr(poly,'Polygons')
		npolys2 = length(polys2)
		for (j in 1:npolys2){
			#do stuff with these values
			coords = sp::coordinates(polys2[[j]])
		}
	}
	
	js <- paste(apply(coords, 1, function (x) paste(x, collapse=",")), collapse="],[")
	
	fullrequest = sprintf('%s?feature_collection={"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Polygon","coordinates":[[[%s]]]},"properties":{}}]}&time=%d&model=%s',url,js, age, model)
	if(verbose) cat("Reconstructing polygon from:",	fullrequest, "\n")
	rawdata <- readLines(fullrequest, warn="F") 
	
	rpoly <- rgdal::readOGR(rawdata, "OGRGeoJSON", verbose = FALSE)
	
	return(rpoly)
}
