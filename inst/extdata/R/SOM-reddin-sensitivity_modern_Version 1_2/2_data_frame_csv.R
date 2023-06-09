#' Function to load in a specific variable
#' @param dir path to archive directory
#' @param verbose  Should feedback be output to the console?
#' @param attach  Should the packages on which the dataset depends be attached? 
#' 
assignInNamespace("loadVar", 
	function(dir, verbose=FALSE, attach=TRUE){
		read.csv(file.path(dir, "Reddin et al. Modern metaanalysis supporting data.csv"))
		
	}, ns="chronosphere"
)
