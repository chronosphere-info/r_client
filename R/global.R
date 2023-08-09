# remote server 
remote <- "https://github.com/chronosphere-portal/chrono_arch_2/raw/main/"
code <- "code/R/"
user <- "public"
registers <- paste0("users/", user, "/")
pwd <- NULL
checklog <- TRUE
curl <- FALSE
timeout <- 300

#' Function to configure the behavior of the chronosphere package.
#'
#' This function will allow you to set package variables.
#'
#' The following package-wide variables can be set.
#' \code{timeout}: Positive integer. Timeout of connections in seconds when the default R sockets are used. The global setting by default is `300`. 
#' #' \code{remote}: Character string. URL of the chronosphere repositories, the default value is \code{"https://github.com/chronosphere-portal/chrono_arch/raw/main/"}.
#' \code{curl}: Logical value. If set to \code{TRUE} (default), the curl libraries are used to download files. If set to \code{FALSE} (default) the default R sockets are used, which can result in timeout issues that need to be resolved manually. 
#' @param remote Character string which is the URL of the chronos server.
#' @param timeout Timeout option used with default R download socket. The global default value for this is 500, which can be increased for very large files and a slow internet connection. Has no effect, when \code{curl=TRUE}.
#' @param curl Logical value of the 'curl' package variable.
#' @examples
#' configure(curl=FALSE)
#' @return The function has no return value.
#' @export
configure<- function(timeout=NULL, remote=NULL, curl=NULL){
	if(!is.null(curl)){
		if(length(curl)!=1) stop("The variable 'curl' has to be either TRUE or FALSE. ")
		if(!is.logical(curl)) stop("The variable 'curl' has to be either TRUE or FALSE. ")
		if(is.na(curl)) stop("The variable 'curl' has to be either TRUE or FALSE. ")
		# rewrite the package variable
		if(requireNamespace("curl", quietly=TRUE)){
			assignInNamespace("curl", curl, ns="chronosphere")
		}else{
			stop("This setting requires the 'curl' R package.\nTry installing with 'install.packages(\"curl\")'.")
		}
	}

	if(!is.null(remote)){
		if(length(remote)!=1) stop("The variable 'remote' has to be a single charater string. ")
		if(!is.character(remote)) stop("The variable 'remote' has to be a single charater string.")
		if(is.na(remote)) stop("The variable 'remote' has to be a single character string. ")
		# rewrite the package variable
		assignInNamespace("remote", remote, ns="chronosphere")
	}

	if(!is.null(timeout)){
		if(length(timeout)!=1) stop("The variable 'timeout' has to be a single positive integer. ")
		if(!is.numeric(timeout)) stop("The variable 'timeout' has to be a single positive integer. ")
		if(timeout%%1!=0) stop("The variable 'timeout' has to be a single positive integer. ")
		if(timeout<=0) stop("The variable 'timeout' has to be a single positive integer. ")
		assignInNamespace("timeout", timeout, ns="chronosphere")

	}
}
