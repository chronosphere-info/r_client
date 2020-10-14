#' Generate data report
#'
#' Generates a report for data documentation from databases and for meta analyses.
#' 
#' @param inputFile (\code{csv}) path to metadata file. The template can be generated using the \code{\link{create_metadata}} function.
#' @param data_refs (\code{data.frame}) data references. See \code{\link{generate_bib}} for more info.
#' @param combine (\code{logical}) Should the generated reference file be combined? See \code{\link{generate_bib}} for more info.
#' @param enterer_names (\code{character}) names of data enterers to be printed at the end of the report
#' @param output_path folder in which to save report
#' @param output_file file name of generated report. Must contain ".pdf" extension
#' @param create_dir (\code{logical}) should a new directory be created in which the report and related files are stored?
#' @param draft (\code{logical}) draft mode allows the editing of the markdown file before the report is generated. Set to \code{FALSE} to generate the file without editing. 
#' @param template (\code{tex}) LaTex file for the formatting of the report. Uses the in-built template by default.
#' @param skeletonFile (\code{Rmd}) Rmd file for the formatting of the report. Uses the in-built template by default. 
#' @param in_header custom additions to the header, before the \code{\\begin{document}} statement
#' @param quiet An option to suppress printing of the pandoc command line.
#'
#' @examples
#' ##Create generic metafile
#' #inputFile <- create_metadata(path=".", edit=FALSE)
#' #
#' ## Download references from the Paleobiology Database
#' #refs <- read.csv("https://paleobiodb.org/data1.2/occs/refs.csv?base_name=Scleractinia&occs_created_after=2020-01-01&select=occs&all_records",
#' #  # encoding required to allow reading of special characters such as accented characters
#' #  encoding="UTF-8")[1:2,]
#' #
#' ## Generate report
#' #report(inputFile=inputFile,
#' #  data_refs = refs,
#' #  output_path = file.path("."),
#' #  output_file = "report.pdf",
#' #  enterer_names=c("Enterer 1", "Enterer 2"))
#'
#' 
#' @export
#'
report <- function(inputFile,
                   data_refs, combine=TRUE,
                   enterer_names=NULL,
                   output_path=".", 
                   output_file="report.pdf",
                   create_dir =TRUE,
                   draft=FALSE,
                   template=pkg_file("rmarkdown", "templates", "template.tex"),
                   skeletonFile=pkg_file("rmarkdown", "templates", "skeleton", "skeleton.Rmd"),
                   in_header = NULL, 
                   quiet = FALSE){
	
	# manage function dependencies
	dependencies <- c("xfun", "kableExtra", "knitr", "yaml", "rmarkdown")
	dependencyFound <- sapply(dependencies, function(x) requireNamespace(x, quietly=TRUE))

	# provide feedback if not found
	if(any(!dependencyFound)) stop(paste0("This function requires the '",  paste(dependencies[!dependencyFound], collapse="', '"), "' package(s) to run."))
	

  # checking parameters -----------------------------------------------------
  if(length(grep("\\.pdf$", output_file)) == 0) {
    stop('Please enter a valid output file name, e.g. \"report.pdf\"')
  }
  
  # create new directory if specified ---------------------------------------
  if(create_dir) {
    output_path <- file.path(output_path, "report")
    dir.create(path=output_path, showWarnings = FALSE)
  }
  
  #read metadata from file
  input <- read.csv(inputFile, header=FALSE, encoding="UTF-8")
  input <- input[input$V1 != "",]
  
  #metadata 
  n <- grep("specs", input[,1])
  
  if(length(n) == 0) {
    warning("No data specifications provided")
    n=nrow(input)+1 #setting end of field
  }
  
  metadata <- input[2:(n-1),]
  
  #contains title and authors?
  pars <- c("title", "authors")
  
  for(p in pars){
    if(length(grep(p, metadata$V1)) == 0| metadata$V2[grep(p, metadata$V1)]=="") warning(sprintf("No %s found", p))
    
  }
  
  metadata <- setNames(split(metadata$V2, seq(nrow(metadata))), metadata$V1)
  
  #authors & affiliations
  metadata[["authors"]] <- strsplit(metadata[["authors"]], ";")[[1]]
  metadata[["affiliation"]] <- strsplit(metadata[["affiliation"]], ";")[[1]]
  
  #add attributes for corresponding author
  metadata$corresauth <- as.numeric(metadata$corresauth)
  
  metadata$authors[metadata$corresauth] <- paste0(metadata$authors[metadata$corresauth], "*")
  metadata$corresauth <- paste("*Corresponding author, email:", metadata$corresemail)
  
  authors <- metadata$authors
  affiliation <- metadata$aff
  
  #multiple authors
  multi <- list()
  
  for(i in 1:length(authors)){
    multi[[i]] <- list(name=authors[i], 
                       affiliation=affiliation[i])
  }
  
  metadata$author <- multi
  
  metadata$affiliation <- metadata$authors <- NULL
  metadata$corresemail <- NULL
  
  specs <- input[(n+1):nrow(input),]
  specs <- setNames(split(specs$V2, seq(nrow(specs))), specs$V1)
  
  metadata$params <- specs
  
  if(!is.null(enterer_names)){
    xfun::write_utf8(paste(enterer_names, collapse = ", "), con=file.path(output_path, "enterernames.txt"))
    metadata$enterer_names <- "enterernames.txt"
  }
  
  #data references
  if(!is.null(data_refs)){
    fname <- generate_bib(data_refs, output_path = output_path, combine=combine)
    metadata$bibliography <- fname
  } else {
    metadata$bibliography <- "references.bib"
  }
  
  
  #save reference for chronosphere
  write(knitr::write_bib("chronosphere", prefix = "R-pkg-")[[1]],
        file=file.path(output_path, metadata$bibliography[1]), append=TRUE)
  
  output_file <- file.path(output_path, output_file)
  
  template_pandoc(metadata=metadata,
                  output_file = output_file, 
                  output_path = output_path,
                  template=template,
                  skeletonFile = skeletonFile,
                  draft=draft,
                  quiet=quiet)
}


#' Generate .bib file from \code{data.frame}
#'
#' @details The \code{generate_bib} function allows conversion of data.frames to BibLaTex entries. Our specifications
#' are based on the tempalte used by the Paleobiology Database. The following entry types are currently supported, with the specified column names:
#' 
#' \itemize{
#' \item{journal article}{ Columns: \code{reftitle}, \code{author}, \code{pubtitle}, \code{pubvol}, \code{page}, \code{pubyear}, \code{doi}}
#' \item{book}{ Columns: \code{pubtitle}, \code{author}, \code{pubyr}, \code{publisher}, \code{pubcity}}
#' \item{book chapter}{ Columns: \code{reftitle}, \code{pubtitle}, \code{author}, \code{editors}, \code{pubyr}, \code{publisher}, \code{pubcity}, \code{page}}
#' \item{book series}{ Columns: \code{reftitle} for the book title, \code{pubtitle} for the series title, \code{author}, \code{pubno}, \code{pubyr}, \code{publisher}, \code{pubcity}}
#' \item{guidebook}{ See \code{book}}
#' \item{thesis}{ Columns: \code{reftitle}, \code{author}, \code{pubyr}, \code{pubtitle} for the institution}
#' \item{misc}{ Columns: \code{reftitle}, \code{author}, \code{pubyr}}
#' \item{abstract}{ See \code{misc}}
#' \item{news article}{ See \code{misc}}
#' \item{compendium}{S ee \code{misc}}
#' \item{unpublished}{ See \code{misc}}
#' }
#' 
#' Note: 
#' \itemize{
#' \item{\code{author} can also be provided as \code{author1init}, \code{author1last}, \code{author2init}, \code{author2last}, \code{otherauthors}}
#' \item{\code{page} can also be provided as \code{firstpage} and \code{lastpage}}
#' }
#' 
#' @param x (\code{data.frame}) references with specific headers. See details for more information. 
#' @param output_path folder where to save generated .bib file. 
#' @param type (\code{character}) type column in \code{x} which specifies the publication type. 
#' @param combine (\code{logical}) Should the references be save on one file or separated in several ones. See details for more information.
#' @param return_fnames (\code{logical}) Should the files names of the generated bib files be returned?
#' @return A list character strings.
#' @export
#'
#' @examples
#' 
#' # refs <- read.csv("https://paleobiodb.org/data1.2/occs/refs.csv?base_name=Scleractinia&occs_created_after=2020-01-01&select=occs&all_records",
#' # # encoding required to allow reading of special characeters, e.g. accents characters such as Ã©, Ã¨
#' # encoding="UTF-8")[1:2,]
#' # 
#' # generate_bib(refs)
#' 
generate_bib <- function (x, output_path=".", type="publication_type", combine=TRUE, return_fnames=TRUE){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  #check author columns
  n <- grep("^author$", colnames(x), fixed=TRUE)
  
  if(length(n) == 0){
    x$author <- NA
    
    for(i in 1:nrow(x)){
      x$author[i] <- auth_gen(x[i,])
    }
  }
  
  #check pages columns
  n <- grep("^page$", colnames(x), fixed=TRUE)
  if(length(n) == 0){
    x$page <- ifelse(is.na(x$firstpage), NA, 
                     ifelse(is.na(x$lastpage), x$firstpage, 
                            paste0(x$firstpage, "-", x$lastpage)))
  }
  
  for(i in 1:nrow(x)){
    cat("\r", i, "out of", nrow(x))
    xx <- x[i,]
    tt <- as.character(xx[,type])
    
    if (tt == "book/book chapter"){
      tt <- ifelse(!((xx$editors == "" & xx$reftitle == "")|
                       (is.na(xx$editors) & is.na(xx$reftitle))), "book chapter", "book")
    }
    
    if(length(grep("thesis", tt)) > 0) tt <- "thesis"
    
    
    bib.df[i,] <- switch(tt,
                         "journal article" = article_bib(xx),
                         "book" = book_bib(xx),
                         "book chapter" = book_ch_bib(xx),
                         "book series" = book_se_bib(xx),
                         "serial monograph" = book_bib(xx),
                         "guidebook" = book_bib(xx),
                         "thesis" = thesis_bib(xx),
                         "abstract" = article_bib(xx),
                         "news article" = article_bib(xx),
                         "compendium" = misc_bib(xx),
                         "misc" = misc_bib(xx),
                         "unpublished" = unpublished_bib(xx)
    )
    
  }
  
  if(combine){
    bib <- df2bib(bib.df)
    # setup file connection
    con <- file(file.path(output_path, "references.bib"), "wb")
    
    xfun::write_utf8(bib, con=con)
    close(con)  # close and destroy a connection
    
    if (return_fnames) return(list("references.bib"))
    
  } else {
    n <- 1:nrow(bib.df)
    
    chunk <- 2000
    
    n <- split(n, ceiling(seq_along(n)/chunk))
    fname <- list()
    
    for(i in seq_along(n)){
      bib <- df2bib(bib.df[n[[i]],])
      fname[[i]] <- sprintf("references%02d.bib", i)
      # setup file connection
      con <- file(file.path(output_path, fname[[i]]), "wb")
      
      xfun::write_utf8(bib, con=con)
      close(con)  # close and destroy a connection
      
    }
    if (return_fnames)return(fname)
  }
}

#' Create metadata file to generate report
#'
#' @param path path where to create file
#' @param edit open file after creation for edits
#' @param overwrite (\code{logical}) Should the file be overwritten if already exists?
#' @param return_path (\code{logical}) return the file path of the create file
#'
#' @examples 
#' # create_metadata(path=".", edit=FALSE)
#' @export
#'
create_metadata <- function(path, edit=TRUE, overwrite=FALSE, return_path=TRUE){
  file.copy(pkg_file("rmarkdown", "templates", "sample.csv"), to=file.path(path, "metadata.csv"))
  
  if (edit) file.show(file.path(path, "metadata.csv"), overwrite=overwrite)
  
  if(return_path) return(file.path(path, "metadata.csv"))
}

# For articles ------------------------------------------------------------
article_bib <- function(xx){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "ARTICLE"
  bib.df$BIBTEXKEY  <- gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  
  bib.df$TITLE <- xx$reftitle
  bib.df$AUTHOR <- xx$author 
  bib.df$JOURNAL <- xx$pubtitle
  bib.df$VOLUME=xx$pubvol
  bib.df$NUMBER=xx$pubno
  bib.df$PAGES=xx$page
  bib.df$YEAR = xx$pubyr 
  bib.df$DOI=xx$doi
  
  bib.df$KEYWORDS="data"
  
  
  return(bib.df)
}


# books -------------------------------------------------------------------
book_bib <- function(xx){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c(
    "CATEGORY",
    "BIBTEXKEY",
    "AUTHOR"   ,
    "BOOKTITLE",
    "EDITOR"       ,
    "INSTITUTION",
    "JOURNAL"    ,
    "NUMBER"  ,
    "PAGES"      ,
    "PUBLISHER"   ,
    "SERIES"       ,
    "TITLE"   ,
    "VOLUME"      ,
    "YEAR"   ,
    "DOI",
    "KEYWORDS"
  )
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "BOOK"
  bib.df$BIBTEXKEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$TITLE = xx$pubtitle
  bib.df$AUTHOR = xx$author
  bib.df$YEAR = xx$pubyr
  bib.df$PUBLISHER = paste(xx$publisher,xx$pubcity)
  bib.df$KEYWORDS ="data"
  
  
  return(bib.df)
}

book_ch_bib <- function(xx){
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "INBOOK"
  bib.df$BIBTEXKEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$BOOKTITLE = xx$pubtitle
  bib.df$TITLE = xx$reftitle
  bib.df$AUTHOR = xx$author
  bib.df$EDITOR=list(strsplit(xx$editors, " ")[[1]])
  bib.df$YEAR = xx$pubyr
  bib.df$PUBLISHER = ifelse(!is.na(xx$publisher) & !is.na(xx$pubcity), paste(xx$publisher,xx$pubcity, sep=", "), NA)
  bib.df$PAGES = xx$page
  bib.df$KEYWORDS="data"
  
  
  return(bib.df)
}

book_se_bib <- function(xx){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "BOOK"
  bib.df$BIBTEXKEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$TITLE = xx$pubtitle
  bib.df$SERIES=xx$pubtitle
  bib.df$NUMBER=xx$pubno
  bib.df$AUTHOR = xx$author
  bib.df$YEAR = xx$pubyr
  bib.df$PUBLISHER = paste(xx$publisher,xx$pubcity, sep=", ")
  bib.df$KEYWORDS="data"
  
  
  return(bib.df)
}

# Thesis ------------------------------------------------------------------
thesis_bib <- function(xx){
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "THESIS"
  bib.df$BIBTEXKEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  
  bib.df$TITLE = xx$reftitle
  bib.df$AUTHOR = xx$author
  bib.df$YEAR = xx$pubyr
  bib.df$INSTITUTION = xx$pubtitle
  
  bib.df$KEYWORDS="data"
  
  
  return(bib.df)
}


# Misc --------------------------------------------------------------------
misc_bib <- function(xx){
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "MISC"
  bib.df$BIBTEXKEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$TITLE = xx$reftitle
  bib.df$AUTHOR = paste(xx$author1init, xx$author1last)
  bib.df$YEAR = xx$pubyr
  bib.df$KEYWORDS="data"
  
  
  return(bib.df)
}

unpublished_bib <- function(xx){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=16))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY",    
                         "AUTHOR"   ,    "BOOKTITLE",   
                         "EDITOR"       ,"INSTITUTION", 
                         "JOURNAL"    ,  "NUMBER"  ,"PAGES"      ,  "PUBLISHER"   ,
                         "SERIES"       ,"TITLE"   ,  
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "UNPUBLISHED"
  bib.df$BIBTEXKEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$TITLE = xx$reftitle
  bib.df$AUTHOR = xx$author
  bib.df$YEAR = xx$pubyr
  bib.df$KEYWORDS="data" 
  
  
  return(bib.df)
}


append_yaml <- function (inputFile, skeletonFile, encoding, yaml) {   
  # read in the YAML + src file
  yaml <- readLines(yaml)
  rmd <- readLines(skeletonFile)
  
  # insert the YAML in after the first ---
  # I'm assuming all my RMDs have properly-formed YAML and that the first
  # occurence of --- starts the YAML. You could do proper validation if you wanted.
  yamlHeader <- grep('^---$', rmd)[1]
  # put the yaml in
  rmd <- append(rmd, yaml, after=yamlHeader)
  
  # write out to a temp file
  ofile <- file.path(tempdir(), basename(inputFile))
  writeLines(rmd, ofile)
  
  # copy back to the current directory.
  file.copy(ofile, file.path(dirname(inputFile), 
                             basename(ofile)), overwrite=T)
}

template_pandoc <- function(metadata, 
                            template,
                            skeletonFile,
                            output_file,
                            output_path,
                            draft,
                            quiet) {
  #create yaml document
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  
  xfun::write_utf8(yaml::as.yaml(metadata), tmp)
  
  append_yaml(gsub("pdf", "Rmd", output_file),
              skeletonFile = skeletonFile,
              yaml=tmp)
  
  #copy template as well
  file.copy(from = template, to=file.path(output_path, basename(template)))
  
  if(draft){
    output_file <- gsub("pdf", "Rmd", output_file)
  } else {
   rmarkdown::render(
      input=gsub("pdf", "Rmd", output_file), quiet=quiet)
    
  }
  
  invisible(output_file)
  file.show(output_file)
}


capitalize <- function(string) {
  paste0(toupper(substr(string, 1, 1)),
         tolower(substr(string, 2, nchar(string))))
}

na_replace <- function(df) {
  df[is.na(df)] <- ""
  return(df)
}

df2bib <- function (x) {
  if (any({
    df_elements <- sapply(x$AUTHOR, inherits, "data.frame")
  })) {
    x$AUTHOR[df_elements] <- lapply(x$AUTHOR[df_elements], 
                                    na_replace)
    x$AUTHOR[df_elements] <- lapply(x$AUTHOR[df_elements], 
                                    function(x) {
                                      paste(x$last_name, ", ", x$first_name, 
                                            " ", x$middle_name, sep = "")
                                    })
    x$AUTHOR[df_elements] <- lapply(x$AUTHOR[df_elements], 
                                    trimws)
  }
  names(x) <- capitalize(names(x))
  fields <- lapply(seq_len(nrow(x)), function(r) {
    rowfields <- rep(list(character(0)), ncol(x))
    names(rowfields) <- names(x)
    for (i in seq_along(rowfields)) {
      f <- x[[i]][r]
      if (is.list(f)) {
        f <- unlist(f)
      }
      rowfields[[i]] <- if (!length(f) || is.na(f)) {
        character(0L)
      }
      else if (names(x)[i] %in% c("Author", "Editor")) {
        paste(f, collapse = " and ")
      }
      else {
        paste0(f, collapse = ", ")
      }
    }
    rowfields <- rowfields[lengths(rowfields) > 0]
    rowfields <- rowfields[!names(rowfields) %in% c("Category", 
                                                    "Bibtexkey")]
    paste0("  ", names(rowfields), " = {", unname(unlist(rowfields)), 
           "}", collapse = ",\n")
  })
  bib <- paste0("@", capitalize(x$Category), "{", 
                x$Bibtexkey, ",\n", unlist(fields), "\n}\n", 
                collapse = "\n")
  return(bib)
}

# multiple author columns to one
auth_gen <- function (x){
  #first author
  x$author <- ifelse(is.na(x$author1init), x$author1last,paste(x$author1init, x$author1last))
  
  #add second author if present
  if(!(x$author2last == ""|is.na(x$author2last))){
    x$author <- paste(x$author, "and",
                      ifelse(is.na(x$author2init), x$author2last, paste(x$author2init, x$author2last)))
  }
  
  #add author others if present
  if(!(x$otherauthors == ""|is.na(x$otherauthors))){
    x$author <-  paste(x$author, "and",
                       gsub(", ", " and ", x$otherauthors))
  }
  #x$author <- iconv(x$author, to="UTF-8")
  x$author <-strsplit(x$author, "and")
  return(x$author)
}