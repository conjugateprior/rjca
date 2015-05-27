##' Drive the category counter
##'
##' Drive the category counter.  See details for possible extra arguments.
##'
##' Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the folder that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary folder}
##'  \item{\code{old.matching}: }{whether to use the old style pattern matching
##'   (behaves like JFreq but also risks double counting when multiple patterns match
##'   a string). Defaults to FALSE}
##'  \item{\code{progress}: }{Whether to show the progress of the java code as it goes
##'  through the \code{files}}
##' }
##' This function also dumps the location of the temporary folder where the
##' results landed to
##' standard error in case you want the original csv data file and yoshikoder dictionary
##' translation.
##'
##' The output is a data.frame with documents as row names, dictionary categories as
##' column names and counts of how many words in each document matched patterns in that
##' category.  Dictionaries are hierarchically structured so category names are sorted
##' depth first and
##'
##' @param dictionary a content analysis dictionary
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a data.frame containing the content analysis results
##' @export
##' @import rJava
jca_cat <- function(dictionary, files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   old.matching=FALSE, progress=TRUE,
                   output=tempfile(pattern="jca_cat"))
  control <- insert_defaults(list(...), defaults)

  cc <- .jnew("org/conjugateprior/ca/app/CategoryCounter")
  .jcall(cc, "V", "setDictionary", path.expand(dictionary))
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setUsingOldMatchStrategy", control$old.matching)
  .jcall(cc, "V", "setSilent", !control$progress)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFolder", control$output)

  .jcall(cc, "V", "processFiles")
  message("Folder is ", control$output)

  ## swap > for _ in the column names
  hh <- read.csv(file.path(control$output, "data.csv"),
                 header=TRUE, row.names=1, check.names=FALSE,
                 encoding="UTF-8")
  names(hh) <- gsub(">", "_", names(hh))
  hh
}

##' Drive the document describer
##'
##' Drive the document describer.  See details for possible extra arguments.
##'
##' Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the file that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary file}
##'  \item{\code{progress}: }{Whether to show the progress of the java code as it goes
##'  through the \code{files}}
##' }
##'
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a data.frame containing brief document descriptions
##' @export
##' @import rJava
jca_desc <- function(files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   progress=TRUE, output=tempfile(pattern="jca_desc",
                                                  fileext=".csv"))
  control <- insert_defaults(list(...), defaults)

  cc <- .jnew("org/conjugateprior/ca/app/Description")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setSilent", !control$progress)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFile", .jnew("java/io/File", control$output))

  .jcall(cc, "V", "processFiles")
  message("File is ", control$output)
  read.csv(control$output, header=TRUE, row.names=1, encoding="UTF-8")
}

##' Determine default locale
##'
##' Determines default locale by parsing the LC_COLLATE locale value
##'
##' @return ISO string representation of a locale
##' @export
get_locale <- function(){
  ll <- Sys.getlocale(category="LC_COLLATE")
  unlist(strsplit(x=ll, split=".", fixed=TRUE))[1]
}

##' Determine default encoding
##'
##' Determines default file encoding by parsing the LC_COLLATE locale value
##'
##' @return string representation of a file encoding
##' @export
get_encoding <- function(){
  ll <- Sys.getlocale(category="LC_COLLATE")
  ff <- unlist(strsplit(x=ll, split=".", fixed=TRUE))[2]
  if (is.na(ff))
    "UTF-8" ## if we can't guess it...
  else
    ff
}

##' Drive the word counter
##'
##' Drive the word counter and return a sparse matrix containing word counts.
##' See details for possible extra arguments.
##'
##' Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the folder that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary folder}
##'  \item{\code{progress}: }{Whether to show the progress of the java code as it goes
##'  through the \code{files}}
##'  \item{\code{no.currency}: }{Whether to remove currency. Defaults to FALSE}
##'  \item{\code{no.numbers}: }{Whether to remove numbers. Defaults to FALSE}
##'  \item{\code{stopwords}: }{File of stop words not to count. By default
##'  every word is counted}
##'  \item{\code{stemmer}: }{A language specific stemmer. By default nothing is stemmed.}
##' }
##'
##' Available stemmer languages are \code{danish}, \code{dutch}, \code{english},
##' \code{finnish}, \code{french}, \code{german}, \code{hungarian}, \code{italian},
##' \code{norwegian}, \code{portuguese}, \code{romanian}, \code{russian},
##' \code{spanish}, \code{swedish}, \code{turkish}.  These are snowball project stemmers
##' so I can't vouch for them.
##'
##' This function also dumps the location of the temporary folder where the
##' results landed to
##' standard error in case you want the original csv data file and yoshikoder dictionary
##' translation.
##'
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a sparse Matrix containing the word counts
##' @export
##' @import rJava
jca_word <- function(files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   no.currency=FALSE, no.numbers=FALSE,
                   stopwords=FALSE, stemmer=FALSE,
                   progress=TRUE, output=tempfile(pattern="jca_word"))
  control <- insert_defaults(list(...), defaults)

  stemlangs <- unlist(strsplit(paste("danish dutch english finnish french german",
                               "hungarian italian norwegian portuguese",
                               "romanian russian spanish swedish turkish"), split=" "))
  if ((control$stemmer != FALSE) && !(tolower(control$stemmer) %in% stemlangs))
    stop("stemmer must be one of:", paste(stemlangs))

  cc <- .jnew("org/conjugateprior/ca/app/WordCounter")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setSilent", !control$progress)
  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFolder", control$output)
  .jcall(cc, "V", "setFormat", "mtx")

  vf <- .jcall(cc, "Lorg/conjugateprior/ca/reports/VocabularyFilterer;", "getFilterer")
  if (control$no.numbers != FALSE)
    .jcall(vf, "V", "addNoNumberFilter")
  if (control$no.currency != FALSE)
    .jcall(vf, "V", "addNoCurrencyFilter")
  if (control$stopwords != FALSE)
    .jcall(vf, "V", "addStopwordFilter", control$stopwords)
  if (control$stemmer != FALSE)
    .jcall(vf, "V", "addStemmingFilter", control$stemmer)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "processFiles")

  message("Folder is ", control$output)
  read_mtx(control$output)
}

#' Read a word frequency matrix in Matrix Market format
#'
#' Returns an sparse Matrix object from a folder generated by \code{jca_word}.
#'
#' The folder is expected to contain 'data.mtx', a word count matrix
#' with documents as rows in Matrix Market format, a file of row names
#' 'documents.csv', and column names 'words.csv'.  Uses the Matrix packages \code{readMM}
#' underneath.
#'
#' @param folder folder containing the data
#' @export
#' @import Matrix
read_mtx <- function(folder){
  ff <- readMM(file.path(folder, "data.mtx"))
  rown <- read.csv(file.path(folder, "docs.csv"), encoding="UTF-8", header=FALSE)$V1
  coln <- read.csv(file.path(folder, "words.csv"), encoding="UTF-8", header=FALSE)$V1
  dimnames(ff) <- list(documents=rown, words=coln)
  ff
}

##' Drive the concordancer
##'
##' Drive the concordancer and return the concordance as a data.frame.
##' See details for possible extra arguments.
##'
##' Possible extra arguments are
##' \itemize{
##'  \item{\code{locale}: }{locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'   whatever \code{get_encoding} returns}
##'  \item{\code{output}: }{the folder that the results will land in before being served
##'   back as a data.frame.  Defaults to a temporary folder}
##'  \item{\code{progress}: }{whether to show the progress of the java code as it goes
##'  through the \code{files}}
##'  \item{\code{dictionary}: }{a content analysis dictionary in ykd or vbpro format}
##'  \item{\code{category}: }{optional name of a category within \code{dictionary}}
##'  \item{\code{pattern}: }{a word or phrase, possible containing wildcard character '*'}
##'  \item{\code{window}: }{how many words either side to show}
##'  \item{\code{prettyprint}: }{Whether to paste an aligned collocation in result data.frame.
##'   Defaults to TRUE}
##'   \item{\code{open.browser}: }{Whether to try to open the concordance as a web page.
##'   Defaults to FALSE}
##' }
##'
##' Either \code{dictionary} or \code{pattern} must be provided.  If \code{dictionary} is
##' provided then \code{category} may also be provided.  If it is not then the concordance
##' is of all the patterns anywhere in the dictionary.
##'
##' This function also dumps the location of the temporary folder where the
##' results landed to standard error in case you want the original html data file
##' from which the data.frame was extracted.
##'
##' When the prettyprint option is FALSE the returned data.frame has three columns. The
##' first is the document from which the collocation line occurs 'Document', the second is the
##' left hand side of the collocation 'LHS' (the \code{window} words leading up to the
##' pattern match), and the third is the pattern match and the \code{window} words
##' succeeding it 'Match.RHS'. Choose this option if e.g. you want to sort the data.frame by
##' match rather than by document, the default.  If you just want to look at the
##' results aligned on the match then the default TRUE setting will be fine.  Then
##' the columns will be 'Document' and 'Concordance'.
##'
##' Note that you can also view the original webpage from which this function
##' scrapes the data.frame.  It is in the folder that the function names
##' and is called 'concordance.html'.  If you set \code{open.browser} to TRUE then R will
##' attempt to open this file in a web browser.
##'
##' Note: Concordances can get big but this function does not try to be efficient.
##'
##' @param files a list of file and folder names
##' @param ... extra arguments to control the process. See Details.
##' @return a data.frame containing
##' @export
##' @import rJava XML
jca_conc <- function(files, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   progress=TRUE, output=tempfile(pattern="jca_conc"),
                   pattern=FALSE, dictionary=FALSE,
                   category=FALSE, window=5,
                   prettyprint=TRUE, open.browser=FALSE)

  control <- insert_defaults(list(...), defaults)
  if (is.null(control$pattern) && is.null(control$dictionary))
    stop("Either a pattern or a dictionary (plus optionally a category) must be set")

  cc <- .jnew("org/conjugateprior/ca/app/Concordancer")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)
  .jcall(cc, "V", "setSilent", !control$progress)
  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFolder", control$output)
  .jcall(cc, "V", "setFormat", "html")
  .jcall(cc, "V", "setWindow", as.integer(control$window))

  if (control$dictionary != FALSE)
    .jcall(cc, "V", "setDictionary", path.expand(control$dictionary))
  if (control$category != FALSE)
    .jcall(cc, "V", "setCategory", control$category)
  if (control$pattern != FALSE)
    .jcall(cc, "V", "addPattern", control$pattern)

  .jcall(cc, "V", "processFiles")
  message("Folder is ", control$output)
  if (control$open.browser)
    browseURL(file.path(control$output, 'concordance.html'))

  df <- readHTMLTable(file.path(control$output, "concordance.html"),
                                header=TRUE, colClasses='character')
  if (control$prettyprint)
    tidy_conc(df$conctable)
  else {
    names(df$conctable) <- c("Document", "LHS", "Match.RHS")
    df$conctable
  }
}

#' Turn jca_conc output into something that looks like a concordance
#'
#' Constructs a data frame with document names in the first column and
#' string-aligned concordance lines in the second column.  Blank document
#' names are filled in as necessary.  Used to format
#' output in \code{jca_conc}.
#'
#' @param df A three column data.frame with incomplete document names
#' @return a pretty data.frame that looks kind of like a concordance
tidy_conc <- function(df){
  dd <- df[,1]
  for (i in 2:length(dd))
    if (dd[i] == "")
      dd[i] <- dd[i-1]

  conc <- as.factor(apply(cbind(format(as.character(df[,2]), justify="right"),
                                  format(as.character(df[,3]), justify="left")),
                            1, function(x){ paste(x[1], x[2])} ))
  data.frame(Document=dd, Concordance=conc)
}

#' Fill in default values for a list
#'
#' Adds entries to a list where necessary.
#'
#' @param lst List to be augmented
#' @param defs Default entry values for \code{lst} if it doesn't already have them
#' @return an augmented list
#'
insert_defaults <- function(lst, defs){
  for (n in names(defs))
    if (is.null(lst[[n]]))
      lst[[n]] <- defs[[n]]
  lst
}

#' Extract counts from a hierarchical dictionary
#'
#' Takes a complete dictionary report and constructs a
#' valid contingency table by slicing the dictionary at `level`.
#' Uses \code{_} as a level marker in the column names.
#'
#' @param df the dictionary output
#' @param level which level of the dictionary to get counts for
#' @param tidy whether to chop off the super category labels in the result
#' @return dictionary results without super or sub category counts
#' @export
dict_slice <- function(df, level=1, tidy=TRUE){
  if (level==0)
    return(df[,1,drop=FALSE]) ## special case

  ff <- function(x){
    # how many levels are marked in this column label?
    length(grep("_", x, value=FALSE, fixed=TRUE))
  }
  indices <- which(sapply(colnames(df), ff)==level)
  dd <- df[,indices]
  if (tidy)
    colnames(dd) <- unlist(lapply(strsplit(colnames(dd), "_"),
                                  function(x){ x[length(x)]}))
  dd
}

##' Drive the single liner
##'
##' Puts a set of documents into a single file in one of the ways Mallet likes.
##' This function creates lines of the form
##'
##' \code{folder-name | NoLabel} tab \code{filename} tab \code{file contents...}
##'
##' This is the format called 'one instance per file' on the Mallet pages
##' (http://mallet.cs.umass.edu/import.php).  If you provide folder names to
##' the \code{files} argument the first line element will be the folder name
##' and the second element will be the names of file within it. If you provide filenames
##' individually then the first line element will be 'NoLabel' and the second element
##' will be the filename.  You can mix these.
##'
##' Note that you cannot provide folders of folders of files.  The function looks for
##' files and folders in \code{files}.  For each of the folders it looks for files and adds
##' them.  Then it stops looking.  Also, files or folders with names beginning
##' with '.' will be ignored.
##'
##' Optional arguments are
##' \itemize{
##'  \item{\code{locale}: }{Locale in ISO format, e.g. "en_US".
##'    Defaults to whatever \code{get_locale} returns}
##'  \item{\code{encoding}: }{the file encoding of the \code{files}. Defaults to the
##'  result of \code{get_encoding}}
##' }
##'
##' @param files a list of file and folder names
##' @param filename the single file you want the contents of \code{files} in
##' @param ... extra arguments to control the process. See Details.
##' @export
##' @import rJava
jca_line <- function(files, filename, ...) {

  defaults <- list(locale=get_locale(), encoding=get_encoding(),
                   progress=TRUE,
                   output=path.expand(filename))
  control <- insert_defaults(list(...), defaults)

  cc <- .jnew("org/conjugateprior/ca/app/SingleLiner")
  .jcall(cc, "V", "setEncoding", control$encoding)
  .jcall(cc, "V", "setLocale", control$locale)

  .jcall(cc, "V", "setFiles", .jarray(sapply(files, path.expand)))
  .jcall(cc, "V", "setOutputFile", .jnew("java/io/File", control$output))

  .jcall(cc, "V", "processFiles")
}


#' rjca: A package for driving jca java code.
#'
#' The rjca package provides functions to count words, count categories
#' using a content analysis dictionary, and generate concordances for words
#' or categories.
#'
#' @docType package
#' @name rjca
NULL
#> NULL

