#' Download a database extract from \code{chronosphere} remote server
#' 
#' The function will download a list of available data from the data repository
#' 
#' The function will download a single .csv file and attach it as a \code{data.frame}.
#' 
#' @param dat \code{character}. Database ID. If this is set to \code{NULL}, then a simplified list of availables variables will be downloaded, including all \code{dat} and \code{var} combinations. If \code{dat} is a valid database ID, then all accessible resolutions and version of a dataset are shown. 
#' @param datadir \code{character} Directory where the downloaded files are kept. Individual entries will be looked up from the directory if this is given, and will be downloaded if they are not found. The default \code{NULL} option will download data to a temporary directory that exists only until the R session ends.
#' @param verbose \code{logical} Should console feedback during download be displayed?
#' @param master \code{logical} When \code{dat} is \code{NULL}, should the function download the master records file?
#' @param greetings \code{logical} When the function is invoked without arguments, it displays a message to keep new users informed about different versions and resolutions (even with \code{verbose=FALSE}). This argument turns this message off on demand.
#' @param all \code{logical} When set to \code{FALSE} (default), only those items are shown that are available for the R environment. Set to \code{TRUE} to see all items.
#' @return A \code{data.frame} class object.
#' @examples
#' # available datasets and variables - proper
#' # index <- datasets()
#' # all available versions and resolutions in database 'pbdb'
#' # oneDat <- datasets("pbdb")
#' ###################################
#' # local example INCOMPLETE - does not connect to the internet
#  # available datasets
#' ind <- datasets(
#'   datadir=system.file("extdata", package="chronosphere"))
#' # one available archive
#' ind <- datasets(
#'   dat="SOM-zaffos-fragmentation",
#'   datadir=system.file("extdata", package="chronosphere"))
#' @export
datasets <- function(dat=NULL, datadir=NULL, verbose=FALSE, master=FALSE, greetings=TRUE, all=FALSE){

	# save timeout parameter from user's global options.
	original<- options()$timeout
	# set to chronosphere global 
	options(timeout=timeout)
	# ensure return to user's original on exit of function
	on.exit(expr=options(timeout=original))
		
	# default case, return the table of variables	
	if(is.null(dat)){
		# simple data table with dat/var combinations
		datfile <- "R/subguide.csv"
		if(master){
			datfile <- "R/submaster.csv"
		}else{
			if(greetings) message("Use datasets(dat = <dat>) to see available versions and resolutions.") 
		}
	}else{
		# recursive call to see whether the dat entry is available
		tempdat <- datasets(datadir=datadir, greetings=FALSE)
	
		if(!any(dat%in%tempdat$dat)) stop(paste0("The dat entry \'", dat, "\' was not found."))

		# full list of available variables in a given dataset - used by fetch()
		datfile <- paste0("R/", dat, ".csv")
	}

	# if it does not exist in datadir, then 
	# by default, download the file
	download <- TRUE

	# a data directory is given
	if(!is.null(datadir)){
		# try to create an R directory to store entries
		dir.create(file.path(datadir, "R"), showWarnings=FALSE)

		# list all files
		allFiles<-file.path("R", list.files(file.path(datadir, "R")))

		# do any of them match? 
		if(any(datfile==allFiles)){
			# read it in
			ret <- read.csv(
				file.path(datadir, datfile), sep=",", 
				header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

			# structure is ok
			if(sum(c("dat", "var")%in%colnames(ret))==2){
				download <- FALSE
			} # no? ->download
			if (verbose) message("Found downloaded registry tables.")
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
		if (verbose) message("Downloading registry tables.")
		# URL to the registry server
		regserv <- paste0(remote, registers )
		#  the public download
		if(user=="public"){
			if(checklog){
				if(curl){
					curl::curl_download(
						paste(regserv, "clientlog.csv", sep = ""),
						tempLog, mode="wb", quiet=TRUE)

				}else{
					download.file(
						paste(regserv, "clientlog.csv", sep = ""),
						tempLog, mode="wb", quiet=TRUE)
				}
			}
			if(curl){
				curl::curl_download(
					paste(regserv, datfile, sep = ""),
					tempReg, mode="wb", quiet=!verbose)

			}else{
				download.file(
					paste(regserv, datfile, sep = ""),
					tempReg, mode="wb", quiet=!verbose)
			}
		# protected download
		}else{
			if(checklog){
				if(curl){
					curl::curl_download(
						paste("ftp://", user, ":", pwd, "@",regserv, "clientlog.csv", sep = ""),
						tempLog, mode="wb", quiet=TRUE)

				}else{
					download.file(
						paste("ftp://", user, ":", pwd, "@",regserv, "clientlog.csv", sep = ""),
						tempLog, mode="wb", quiet=TRUE)
				}
			}
			
			if(curl){
				curl::curl_download(
					paste("ftp://", user, ":", pwd, "@",regserv, datfile, sep = ""),
					tempReg, mode="wb", quiet=!verbose)

			}else{
				download.file(
					paste("ftp://", user, ":", pwd, "@",regserv, datfile, sep = ""),
					tempReg, mode="wb", quiet=!verbose)
			}
		}	
		
	
		# check the server log.
		if(checklog){
			# read server log
			log <- tryCatch(
				read.csv(
					tempLog, sep=",", header=TRUE, 
					stringsAsFactors=FALSE, encoding="UTF-8"), 
				error=function() 
					stop("Invalid log file, remote server cannot be reached.")
			)

			# subset the table to the appropriate client
			log <- log[log$client=="R", ]
			pkgver <- sessionInfo()$otherPkgs$chronosphere$Version
			bLine <- pkgver==log$version

			# Keys:
			# version not shown: everything fine!
			# version found: display message
			# display message intended for people using this particular version
			currentMessage <- log$message[bLine]

			logok <- TRUE 
			if(length(currentMessage)!=0){
				warning(currentMessage)
				logok <- FALSE
			}
			if(logok) assignInNamespace("checklog", FALSE, ns="chronosphere")
			unlink(tempLog)
		}
		
		# and set return value
		ret <- read.csv(
			tempReg, sep=",", header=TRUE, 
			stringsAsFactors=FALSE, encoding="UTF-8")

		# show only the R-items
		if(!is.null(dat) & !all) ret <- ret[ret$language=="R", ]
		# get rid of the  temporary file
		if(is.null(datadir)) unlink(tempReg)
		
	}
	if(master){
		ret$codeURL <- file.path(remote,"code/R",ret$codeFile)
	}

	return(ret)
}
