#' Generate report
#'
#' @export 
generate_report <- function(metadata=list(title="Sample title",
                                          authors=c("Jane Doe", "John Doe"),
                                          corresauth=1,
                                          corresemail="jane.doe@fau.de",
                                          affiliation=c("Friedrich Alexander Universität", "University of Birmingham"),
                                          abstract="This is where my abstract goes...",
                                          keywords="data, paleobiology, fossil"),
                            specs=list(source = "The Paleobiology Database",
                                       url = "https://paleobiodb.org/data1.2/occs/list.csv?datainfo&rowcount&interval=
                            Ediacaran,Holocene&show=class,classext,genus,subgenus,coll,coords,loc,paleoloc,
                            strat,stratext,lith,env,ref,crmod,timebins,timecompare",
                                       download_date = "2020-06-04",
                                       dataformat = "Processed",
                                       dataaccess = "Available with this report",
                                       taxa = "Multiple",
                                       location = "Global",
                                       time = "Cambrian to Recent"),
                            data_refs,
                            enterer_names=NULL,
                            output_path=".", 
                            output_file="test.pdf",
                            create_dir =TRUE,
                            template=pkg_file("rmarkdown", "templates", "template.tex"),
                            skeletonFile=pkg_file("rmarkdown", "templates", "skeleton", "skeleton.Rmd"),
                            in_header = NULL, 
                            verbose = FALSE){
  if(create_dir) {
    output_path <- file.path(output_path, "report")
    dir.create(output_path)
  }
  
  metadata$authors[metadata$corresauth] <- paste0(metadata$authors[metadata$corresauth], "*")
  metadata$corresauth <- paste("*Corresponding author, email:", metadata$corresemail)
  
  metadata$corresemail <- NULL
  
  #change encoding to allow special characters
  metadata <- lapply(metadata, function(x) iconv(x, from="latin1", to="utf8"))
  
  metadata$params <- specs
  
  if(!is.null(enterer_names)){
    write(paste(enterer_names, collapse = ", "), file=file.path(output_path, "enterernames.txt"))
    metadata$enterer_names <- "enterernames.txt"
  }
  
  #data references
  bib <- generate_bib(data_refs)
  
  xfun::write_utf8(RefManageR::toBiblatex(bib), 
                   file.path(output_path, "references.bib"))
  
  #save reference to chronosphere
  write(knitr::write_bib("chronosphere", prefix = "R-pkg-")[[1]], 
        file=file.path(output_path,"references.bib"), 
        append=TRUE)
  
  output_file <- file.path(output_path, output_file)
  
  template_pandoc(metadata=metadata,
                  output_file = output_file, 
                  output_path = output_path,
                  template=template,
                  skeletonFile = skeletonFile)
}

#' Generate bibliography
#'
#' @export 
generate_bib <- function (x, type="publication_type"){
  x$author <- paste(paste(x$author1init, x$author1last), "and",
                    paste(x$author2init, x$author1last), "and",
                    gsub(", ", " and ", x$otherauthors)
  )
  x$page <- ifelse(x$firstpage == "", NA, paste0(x$firstpage, "-", x$lastpage))
  
  
  for(i in 1:nrow(x)){
    cat("\r", i, "out of", nrow(x))
    xx <- x[i,]
    tt <- xx[,type]
    
    if (tt == "book/book chapter"){
      tt <- ifelse(xx$editors != "" & xx$reftitle != "", "book chapter", "book")
    }
    
    if(length(grep("thesis", tt)) > 0) tt <- "thesis"
    
    if (i == 1){
      bib <- switch(tt,
                    "journal article" = article_bib(xx),
                    "book" = book_bib(xx),
                    "book chapter" = book_ch_bib(xx),
                    "serial monograph" = book_bib(xx),
                    "guidebook" = book_bib(xx),
                    "thesis" = thesis_bib(xx),
                    "abstract" = article_bib(xx),
                    "news article" = article_bib(xx),
                    "compendium" = misc_bib(xx),
                    "misc" = misc_bib(xx),
                    "unpublished" = unpublished_bib(xx)
      ) 
      
    } else {
      tryCatch({bib[[i]] <- switch(tt,
                                   "journal article" = article_bib(xx),
                                   "book" = book_bib(xx),
                                   "book chapter" = book_ch_bib(xx),
                                   "serial monograph" = book_bib(xx),
                                   "guidebook" = book_bib(xx),
                                   "thesis" = thesis_bib(xx),
                                   "abstract" = article_bib(xx),
                                   "news article" = article_bib(xx),
                                   "compendium" = misc_bib(xx),
                                   "misc" = misc_bib(xx),
                                   "unpublished" = unpublished_bib(xx)
      )
      }, error = function (e) {message (i, ": Invalid entry -", e); break})
    }
  }
  return(bib) 
}

# For articles ------------------------------------------------------------
article_bib <- function(xx){
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib <- RefManageR::BibEntry(bibtype = "article", 
                              key = gsub(" ", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff)), 
                              title = xx$reftitle,
                              author = xx$author, 
                              journaltitle = xx$pubtitle,
                              volume=xx$pubvol,
                              number=xx$pubno,
                              page=xx$page,
                              date = xx$pubyr, 
                              doi=xx$doi,
                              keywords="data")  
  
  
  return(bib)
}


# books -------------------------------------------------------------------
book_bib <- function(xx){
  
  bib <- RefManageR::BibEntry(bibtype = "book", 
                              key = paste0(trimws(xx$author1last), xx$pubyr), 
                              title = xx$pubtitle,
                              author = xx$author,
                              year = xx$pubyr,
                              publisher = xx$publisher,
                              place = xx$pubcity,
                              keywords="data")  
  
  
  return(bib)
}

book_ch_bib <- function(xx){
  
  bib <- RefManageR::BibEntry(bibtype = "inbook", 
                              key = paste0(trimws(xx$author1last), xx$pubyr), 
                              booktitle = xx$pubtitle,
                              title = xx$reftitle,
                              author = xx$author,
                              editor=gsub(",", "and", xx$editors),
                              year = xx$pubyr,
                              publisher = xx$publisher,
                              page = xx$page,
                              place = xx$pubcity,
                              keywords="data")  
  
  
  return(bib)
}


# Thesis ------------------------------------------------------------------
thesis_bib <- function(xx){
  
  bib <- RefManageR::BibEntry(bibtype = "thesis", 
                              key = paste0(trimws(xx$author1last), xx$pubyr),
                              title = xx$reftitle,
                              author = paste(xx$author1init, xx$author1last),
                              year = xx$pubyr,
                              institution = xx$pubtitle,
                              type=xx$publication_type,
                              keywords="data")  
  
  
  return(bib)
}


# Misc --------------------------------------------------------------------
misc_bib <- function(xx){
  
  bib <- RefManageR::BibEntry(bibtype = "misc", 
                              key = paste0(trimws(xx$author1last), xx$pubyr),
                              title = xx$reftitle,
                              author = paste(xx$author1init, xx$author1last),
                              year = xx$pubyr,
                              keywords="data")  
  
  
  return(bib)
}

unpublished_bib <- function(xx){
  
  bib <- RefManageR::BibEntry(bibtype = "unpublished", 
                              key = paste0(trimws(xx$author1last), xx$pubyr),
                              title = xx$reftitle,
                              author = paste(xx$author1init, xx$author1last),
                              year = xx$pubyr,
                              keywords="data")  
  
  
  return(bib)
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
                            output_path) {
  #create yaml document
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  
  #multiple authors
  authors <- metadata$authors
  affiliation <- metadata$aff
  
  multi <- list()
  
  for(i in 1:length(authors)){
    multi[[i]] <- list(name=authors[i], 
                       affiliation=affiliation[i])
  }
  
  metadata$author <- multi
  
  metadata$aff <- metadata$authors <- NULL
  
  xfun::write_utf8(yaml::as.yaml(metadata), tmp)
  
  append_yaml(gsub("pdf", "Rmd", output_file),
              skeletonFile = skeletonFile,
              yaml=tmp)
  #copy template as well
  file.copy(from = template, to=file.path(output_path, basename(template)))
  
  rmarkdown::render(
    input=gsub("pdf", "Rmd", output_file))
  
  invisible(output_file)
  file.show(output_file)
}
