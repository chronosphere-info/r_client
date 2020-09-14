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
  metadata <- lapply(metadata, function(x) iconv(x, to="UTF-8"))
  
  metadata$params <- specs
  
  if(!is.null(enterer_names)){
    write(paste(enterer_names, collapse = ", "), file=file.path(output_path, "enterernames.txt"))
    metadata$enterer_names <- "enterernames.txt"
  }
  
  #data references
  bib <- generate_bib(data_refs, output_path = output_path)
  
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
generate_bib <- function (x, type="publication_type", output_path){
  
  bib.df <- data.frame(matrix(NA, nrow=nrow(x), ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
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
  x$author <- NA
  
  for(i in 1:nrow(x)){
    x$author[i] <- auth_gen(x[i,])
  }
  
  x$page <- ifelse(is.na(x$firstpage), NA, 
                   ifelse(is.na(x$lastpage), x$firstpage, 
                                paste0(x$firstpage, "-", x$lastpage)))
  
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
  
  
  bib <- df2bib(bib.df)
    
  xfun::write_utf8(bib, file.path(output_path, "references.bib"))
}

# For articles ------------------------------------------------------------
article_bib <- function(xx){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
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
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "BOOK"
  bib.df$KEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$TITLE = xx$pubtitle
  bib.df$AUTHOR = xx$author
  bib.df$YEAR = xx$pubyr
  bib.df$PUBLISHER = paste(xx$publisher,xx$pubcity)
  bib.df$KEYWORDS ="data"
  
  
  return(bib.df)
}

book_ch_bib <- function(xx){
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "INBOOK"
  bib.df$KEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
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
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "BOOK"
  bib.df$KEY = gsub(" |[^A-z]", "", paste0(trimws(unlist(strsplit(xx$author1last, " "))[1]), xx$pubyr, suff))
  bib.df$TITLE = xx$reftitle
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
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "THESIS"
  
  bib.df$TITLE = xx$reftitle
  bib.df$AUTHOR = paste(xx$author1init, xx$author1last)
  bib.df$AUTHOR = xx$pubyr
  bib.df$INSTITUTION = xx$pubtitle
  
  bib.df$KEYWORDS="data"
  
  
  return(bib.df)
}


# Misc --------------------------------------------------------------------
misc_bib <- function(xx){
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "MISC"
  
  bib.df$TITLE = xx$reftitle
  bib.df$AUTHOR = paste(xx$author1init, xx$author1last)
  bib.df$YEAR = xx$pubyr
  bib.df$KEYWORDS="data"
  
  
  return(bib)
}

unpublished_bib <- function(xx){
  
  bib.df <- data.frame(matrix(NA, nrow=1, ncol=28))
  colnames(bib.df) <- c( "CATEGORY",     "BIBTEXKEY"  ,  "ADDRESS"   ,   "ANNOTE"  ,    
                         "AUTHOR"   ,    "BOOKTITLE"   , "CHAPTER"    ,  "CROSSREF" ,   
                         "EDITION"   ,   "EDITOR"       ,"HOWPUBLISHED", "INSTITUTION", 
                         "JOURNAL"    ,  "KEY"          ,"MONTH"        ,"NOTE"       , 
                         "NUMBER"      , "ORGANIZATION" ,"PAGES"      ,  "PUBLISHER"   ,
                         "SCHOOL"     ,  "SERIES"       ,"TITLE"      ,  "TYPE"        ,
                         "VOLUME"      , "YEAR"   , "DOI", "KEYWORDS")
  
  suff <- paste(sample(letters, 4), collapse = "")
  bib.df$CATEGORY <- "UNPUBLISHED"
  
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

capitalize <- function(string) {
  paste0(substr(string, 1, 1),
         tolower(substr(string, 2, nchar(string))))
}

na_replace <- function(df) {
  df[is.na(df)] <- ""
  return(df)
}

df2bib <- function (x) 
{
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
                collapse = "\n\n")
  return(bib)
}

